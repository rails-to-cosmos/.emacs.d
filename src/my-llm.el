;;; my-llm.el --- Claude CLI integration for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'f)
(require 'json)

(require 'my-terminal)

;;; Variables — user settings (global)

(defgroup llm nil
  "Claude CLI integration for Emacs."
  :group 'tools
  :prefix "llm-")

(defcustom llm-model "sonnet"
  "Current Claude model: \"sonnet\", \"opus\", or \"haiku\"."
  :type '(choice (const "sonnet") (const "opus") (const "haiku"))
  :group 'llm)

(defcustom llm-claude-executable "claude"
  "Path to the Claude CLI executable."
  :type 'string
  :group 'llm)

(defcustom llm-system-prompt nil
  "Optional custom system prompt. Nil means use CLI default."
  :type '(choice (const nil) string)
  :group 'llm)

(defcustom llm-max-turns nil
  "Optional max agentic turns. Nil means no limit."
  :type '(choice (const nil) natnum)
  :group 'llm)

(defvar llm--project-root-markers '(".git" ".claude" "CLAUDE.md")
  "Files/dirs that indicate a project root.")

(defvar llm--prompt-keyword "PROMPT"
  "Org TODO keyword used as the heading state for prompt entries in the output buffer.
Changing this also changes the text written and the regex used to locate those headings.")

;;; Buffer-local variables

(defvar-local llm--buffer-project-root nil
  "Project root associated with this output buffer.")

(defvar-local llm--prompt-project-root nil
  "Project root captured when the prompt buffer was opened.")

;;; Per-project state

(defvar llm--projects (make-hash-table :test 'equal)
  "Hash table mapping project root strings to state plists.
Each plist holds :process, :session-id, :streaming-buffer, :region-indicator.
:region-indicator is a plist with :buf-name (string) when the mode-line
indicator is active, or nil when none is shown.")

(defun llm--project-state (&optional root)
  "Return or create state plist for ROOT (defaults to `llm--project-root')."
  (let ((r (or root (llm--project-root))))
    (or (gethash r llm--projects)
        (let ((state (list :process nil :session-id nil
                           :streaming-buffer "" :region-indicator nil
                           :temp-files nil)))
          (puthash r state llm--projects)
          state))))

(defun llm--project-get (key &optional root)
  "Get KEY from the project state for ROOT."
  (plist-get (llm--project-state root) key))

(defun llm--project-set (key value &optional root)
  "Set KEY to VALUE in the project state for ROOT."
  ;; llm--project-state creates and stores the plist on first access;
  ;; plist-put mutates it in place thereafter, so puthash is not needed.
  (plist-put (llm--project-state (or root (llm--project-root))) key value)
  value)

;;; Project Root Detection

(cl-defun llm--project-root (&optional (dir default-directory))
  "Find project root starting from DIR by looking for marker files."
  (or (cl-loop for marker in llm--project-root-markers
               for root = (locate-dominating-file dir marker)
               when root return (file-name-as-directory root))
      (file-name-as-directory dir)))

(defun llm--current-root ()
  "Get the project root for the current context.
Uses buffer-local roots if set, otherwise determines from `default-directory'."
  (or llm--buffer-project-root
      llm--prompt-project-root
      (llm--project-root)))

;;; Indicator Faces

(defface llm-indicator-pending-face
  '((t :foreground "gray50"))
  "Face for the status indicator when Claude is thinking.")

(defface llm-indicator-streaming-face
  '((t :foreground "dodger blue"))
  "Face for the status indicator when Claude is streaming (reserved for future use).")

(defface llm-indicator-done-face
  '((t :foreground "green3"))
  "Face for the status indicator when Claude has finished.")

(defface llm-indicator-error-face
  '((t :foreground "red"))
  "Face for the status indicator when the process errored.")

(defface llm-indicator-permission-face
  '((t :foreground "dark orange"))
  "Face for when Claude is awaiting tool permission.")

;;; Indicator Functions — mode-line based

(defvar llm--mode-line-indicators nil
  "Alist of (root . propertized-string) for every active LLM request.
Entries are added by `llm--region-indicator-create' and removed by
`llm--region-indicator-delete'.  The mode-line segment reads this alist
via `llm--mode-line-segment'.")

(defun llm--mode-line-segment ()
  "Return the mode-line indicator for the current buffer's project, or nil.
Only buffers whose project root matches an active LLM request show an indicator."
  (when llm--mode-line-indicators
    (when-let ((ind (alist-get (llm--current-root)
                               llm--mode-line-indicators nil nil #'equal)))
      (concat " " ind))))

;; Register the segment in global-mode-string exactly once.
(cl-pushnew '(:eval (llm--mode-line-segment)) global-mode-string :test #'equal)

(defun llm--indicator-string (buf-name status face)
  "Return a propertized mode-line string for BUF-NAME, STATUS, and FACE.
Each string carries its own keymap so that a mouse click opens exactly
the buffer named BUF-NAME regardless of which window was active."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda () (interactive)
        (if (get-buffer buf-name)
            (pop-to-buffer buf-name)
          (user-error "Buffer %s no longer exists" buf-name))))
    (propertize (format "[%s : %s]" buf-name status)
                'face face
                'mouse-face 'highlight
                'keymap map
                'help-echo (format "mouse-1: open %s" buf-name))))

(defun llm--region-indicator-create (face &optional root)
  "Show a status indicator in the mode-line for ROOT."
  (let* ((root (or root (llm--project-root)))
         (buf-name (buffer-name (llm--output-buffer root))))
    (llm--region-indicator-delete root)          ; clear any stale entry
    (let ((str (llm--indicator-string buf-name "thinking" face)))
      (push (cons root str) llm--mode-line-indicators)
      (llm--project-set :region-indicator (list :buf-name buf-name) root))
    (force-mode-line-update t)))

(defun llm--region-indicator-update (status &optional face root)
  "Update the mode-line indicator to show STATUS with FACE for ROOT."
  (let ((ind (llm--project-get :region-indicator root)))
    (when ind
      (let* ((r (or root (llm--project-root)))
             (buf-name (plist-get ind :buf-name))
             (str (llm--indicator-string buf-name status
                                         (or face 'llm-indicator-pending-face))))
        (setf (alist-get r llm--mode-line-indicators nil nil #'equal) str)
        (force-mode-line-update t)))))

(defun llm--region-indicator-delete (&optional root)
  "Remove the mode-line indicator for ROOT if present."
  (let ((r (or root (llm--project-root))))
    (setq llm--mode-line-indicators
          (cl-remove r llm--mode-line-indicators :key #'car :test #'equal))
    (llm--project-set :region-indicator nil root)
    (force-mode-line-update t)))

;;; Output Mode

(defvar llm-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'llm)
    (define-key map "m" #'llm-select-model)
    (define-key map "k" #'llm-stop)
    (define-key map "n" #'llm-new-session)
    (define-key map "c" #'llm-copy-response)
    (define-key map "q" #'quit-window)
    (define-key map "a" #'llm-permission-allow)
    (define-key map "d" #'llm-permission-deny)
    map)
  "Keymap for `llm-output-mode'.")

(define-derived-mode llm-output-mode org-mode "LLM"
  "Major mode for displaying Claude CLI output as org."
  (setq-local org-todo-keywords
              (list (list 'sequence llm--prompt-keyword "|" "DONE")))
  (setq buffer-read-only t)
  (visual-line-mode 1))

;;; Permission Mode

(defvar llm-permission-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "y" #'llm-permission-allow)
    (define-key map "n" #'llm-permission-deny)
    map)
  "Keymap for `llm-permission-mode'.
Active only when a permission prompt is pending.")

(define-minor-mode llm-permission-mode
  "Minor mode active when Claude CLI is awaiting tool permission.
Binds `y' to allow and `n' to deny."
  :lighter " Perm"
  :keymap llm-permission-mode-map)

(defun llm--permission-respond (answer label)
  "Send ANSWER (\"y\\n\" or \"n\\n\") to the pending permission prompt.
Inserts LABEL into the output buffer and cleans up permission state."
  (let* ((root (llm--current-root))
         (proc (llm--project-get :process root)))
    (unless (llm--project-get :permission-pending root)
      (user-error "No permission pending"))
    (when (and proc (process-live-p proc))
      (process-send-string proc answer))
    (llm--insert-output (format "\n[%s]\n" label) root)
    (llm--project-set :permission-pending nil root)
    (llm-permission-mode -1)
    (llm--region-indicator-update "streaming" 'llm-indicator-streaming-face root)))

(defun llm-permission-allow ()
  "Allow the pending tool permission."
  (interactive)
  (llm--permission-respond "y\n" "allowed"))

(defun llm-permission-deny ()
  "Deny the pending tool permission."
  (interactive)
  (llm--permission-respond "n\n" "denied"))

;;; Prompt Mode

(defvar llm-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'llm-prompt-send)
    (define-key map (kbd "C-c C-k") #'llm-prompt-cancel)
    map)
  "Keymap for `llm-prompt-mode'.")

(define-derived-mode llm-prompt-mode text-mode "LLM-Prompt"
  "Major mode for composing multi-line Claude prompts.
\\[llm-prompt-send] to send, \\[llm-prompt-cancel] to cancel."
  (setq header-line-format
        (list " Claude [" 'llm-model "]  C-c C-c send | C-c C-k cancel")))

;;; Output Buffer — History Persistence

(defun llm--history-file (root)
  "Return the path to the persistent org history file for ROOT."
  (expand-file-name "llm-history.org" root))

(defun llm--save-history (&optional root)
  "Write the output buffer for ROOT to its llm-history.org file."
  (let* ((r (or root (llm--project-root)))
         (buf (get-buffer (llm--output-buffer-name r))))
    (when buf
      (with-current-buffer buf
        (write-region (point-min) (point-max)
                      (llm--history-file r) nil 'silent)))))

;;; Output Buffer

(defun llm--output-buffer-name (root)
  "Return the *llm:parent/dir* buffer name for ROOT.
Uses the last two path components so projects sharing a leaf name
\(e.g. two different `src' dirs) get distinct buffer names."
  (let* ((dir  (directory-file-name root))
         (leaf (file-name-nondirectory dir))
         (up   (file-name-nondirectory (directory-file-name
                                        (file-name-directory dir)))))
    (if (string-empty-p up)
        (format "*llm:%s*" leaf)
      (format "*llm:%s/%s*" up leaf))))

(defun llm--output-buffer (&optional root)
  "Return the project's *llm:parent/dir* output buffer, creating it if needed.
ROOT defaults to `llm--project-root'.  On first creation the buffer is
populated from the project's llm-history.org file if one exists, and a
kill-buffer hook is registered to save it back on close."
  (let* ((r (or root (llm--project-root)))
         (buf-name (llm--output-buffer-name r))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'llm-output-mode)
        (llm-output-mode)
        ;; Restore persisted history if available.
        (let ((file (llm--history-file r)))
          (when (file-readable-p file)
            (let ((inhibit-read-only t))
              (insert-file-contents file))))
        ;; Save to disk whenever the buffer is killed.
        (add-hook 'kill-buffer-hook
                  (lambda () (llm--save-history r))
                  nil t))
      (setq-local llm--buffer-project-root r))
    buf))

(defun llm--insert-output (text &optional root)
  "Insert TEXT into the output buffer for ROOT at the end."
  (let ((buf (llm--output-buffer root)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (point-max))
          (insert text))
        (when at-end
          (goto-char (point-max)))))
    (when-let ((win (get-buffer-window buf)))
      (with-selected-window win
        (goto-char (point-max))
        (recenter -1)))))

(defun llm--insert-prompt-header (prompt &optional root)
  "Insert a prompt header line for PROMPT into the output buffer for ROOT."
  (llm--insert-output (format "* %s %s\n\n" llm--prompt-keyword prompt) root))

(defun llm--insert-footer (session-id cost model &optional root)
  "Insert SESSION-ID, COST, and MODEL as PROPERTIES on the main prompt header for ROOT."
  (let ((props nil))
    (when session-id (push (cons "session" session-id) props))
    (when cost       (push (cons "cost"    (format "$%s" cost)) props))
    (when model      (push (cons "model"   model) props))
    (when props
      (let ((buf (llm--output-buffer root)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (when (re-search-backward
                     (concat "^\\* " (regexp-quote llm--prompt-keyword) "\\b") nil t)
                (end-of-line)
                (insert "\n:PROPERTIES:")
                (dolist (pair (nreverse props))
                  (insert (format "\n:%-10s %s" (concat (car pair) ":") (cdr pair))))
                (insert "\n:END:")))))))))

;;; Streaming Process

(defvar llm--org-format-instruction
  "Format all responses in Emacs org-mode syntax. Use org headings (** ***), ~code~, =verbatim=, *bold*, /italic/, and #+begin_src/#+end_src blocks for code. Never use markdown syntax."
  "System prompt fragment instructing org-mode output format.")

(defun llm--build-args (prompt &optional flags)
  "Build argument list for the Claude CLI with PROMPT and optional FLAGS.
FLAGS is a list of extra CLI arguments."
  (let ((args (append (list "-p"
                            "--output-format" "stream-json"
                            "--model" llm-model
                            "--permission-mode" "acceptEdits"
                            "--verbose")
                      flags)))
    (let ((sys-prompt (if llm-system-prompt
                         (concat llm--org-format-instruction "\n\n" llm-system-prompt)
                       llm--org-format-instruction)))
      (setq args (append args (list "--system-prompt" sys-prompt))))
    (when llm-max-turns
      (setq args (append args (list "--max-turns" (number-to-string llm-max-turns)))))
    ;; Use -- to separate options from the positional prompt,
    ;; since variadic flags like --allowedTools consume all following args
    (when prompt
      (setq args (append args (list "--" prompt))))
    args))

(defun llm--handle-json-line (line &optional root)
  "Parse and handle a single JSON LINE from Claude CLI stream for ROOT."
  (condition-case nil
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-false nil)
             (data (json-read-from-string line))
             (type (alist-get 'type data)))
        ;; Valid JSON arrived — clear any pending permission state
        (when (llm--project-get :permission-pending root)
          (llm--project-set :permission-pending nil root)
          (let ((buf (llm--output-buffer root)))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (when llm-permission-mode
                  (llm-permission-mode -1))))))
        (cond
         ((string= type "assistant")
          (let* ((message (alist-get 'message data))
                 (content (alist-get 'content message)))
            (dolist (block content)
              (when (string= (alist-get 'type block) "text")
                (llm--insert-output (alist-get 'text block) root)))))
         ((string= type "result")
          (let ((session-id (alist-get 'session_id data))
                (cost (alist-get 'total_cost_usd data)))
            (when session-id
              (llm--project-set :session-id session-id root))
            (llm--insert-footer session-id cost llm-model root)))
         ((string= type "system")
          (let ((session-id (alist-get 'session_id data)))
            (when session-id
              (llm--project-set :session-id session-id root))))))
    (json-error (llm--handle-non-json-line line root))))

(defun llm--handle-non-json-line (line &optional root)
  "Handle a non-JSON LINE from the Claude CLI stream for ROOT.
Strips box-drawing characters, inserts into output buffer, and
activates permission mode if a permission prompt is detected."
  (let ((cleaned (replace-regexp-in-string "[╭╮╰╯│─]" "" line)))
    (unless (string-empty-p (string-trim cleaned))
      (llm--insert-output (concat (string-trim cleaned) "\n") root)
      (when (string-match-p "(y)\\|(n)\\|Allow" cleaned)
        (llm--project-set :permission-pending t root)
        (llm--region-indicator-update "permission" 'llm-indicator-permission-face root)
        (let ((buf (llm--output-buffer root)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (unless llm-permission-mode
                (llm-permission-mode 1)))))))))

(defun llm--strip-ansi (str)
  "Remove ANSI escape sequences and carriage returns from STR."
  (replace-regexp-in-string "\r\\|\e\\[[0-9;?]*[a-zA-Z]" "" str))

(defun llm--process-filter (process output)
  "Filter function for Claude CLI PROCESS. Accumulates OUTPUT, parses JSON lines."
  (let ((root (process-get process :llm-project-root)))
    (llm--project-set :streaming-buffer
                      (concat (llm--project-get :streaming-buffer root)
                              (llm--strip-ansi output))
                      root)
    (let ((lines (split-string (llm--project-get :streaming-buffer root) "\n")))
      (llm--project-set :streaming-buffer (car (last lines)) root)
      (dolist (line (butlast lines))
        (unless (string-empty-p (string-trim line))
          (llm--handle-json-line (string-trim line) root))))))

(defun llm--process-sentinel (process _event)
  "Sentinel for Claude CLI PROCESS."
  (let ((root (process-get process :llm-project-root)))
    (when (eq process (llm--project-get :process root))
      (let ((exit-code (process-exit-status process)))
        (llm--project-set :process nil root)
        (unless (= exit-code 0)
          (llm--insert-output (format "\n[process exited with code %d]\n" exit-code) root))
        (when (llm--project-get :region-indicator root)
          (if (= exit-code 0)
              (llm--region-indicator-update "done" 'llm-indicator-done-face root)
            (llm--region-indicator-update "error" 'llm-indicator-error-face root))))
      ;; Clean up permission state if still pending
      (when (llm--project-get :permission-pending root)
        (llm--project-set :permission-pending nil root)
        (let ((buf (llm--output-buffer root)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (when llm-permission-mode
                (llm-permission-mode -1))))))
      ;; Process any remaining buffered data
      (let ((buf (llm--project-get :streaming-buffer root)))
        (when (and buf (not (string-empty-p (string-trim buf))))
          (llm--handle-json-line (string-trim buf) root)))
      (llm--project-set :streaming-buffer "" root)
      ;; Delete temporary context files created for this session
      (dolist (f (llm--project-get :temp-files root))
        (ignore-errors (delete-file f)))
      (llm--project-set :temp-files nil root)
      ;; Persist the output buffer to the project's history file.
      (llm--save-history root))))

(defun llm--start-process (prompt &optional flags show-buffer root)
  "Start a Claude CLI process with PROMPT and optional FLAGS for ROOT.
When SHOW-BUFFER is non-nil, switch to the output buffer."
  (let* ((root (or root (llm--project-root)))
         (proc (llm--project-get :process root)))
    (when (and proc (process-live-p proc))
      (user-error "Claude process already running — use `llm-stop' first"))
    (llm--project-set :streaming-buffer "" root)
    (llm--region-indicator-create 'llm-indicator-pending-face root)
    (let* ((default-directory root)
           (args (llm--build-args prompt flags))
           ;; Clear CLAUDECODE env var so CLI doesn't refuse nested sessions
           (process-environment (cl-remove-if
                                 (lambda (s) (string-match-p "\\`CLAUDECODE=" s))
                                 process-environment))
           (proc (make-process
                  :name "claude-cli"
                  :buffer nil
                  :command (cons llm-claude-executable args)
                  :filter #'llm--process-filter
                  :sentinel #'llm--process-sentinel
                  :connection-type 'pty
                  :noquery t)))
      (process-put proc :llm-project-root root)
      (llm--project-set :process proc root)
      (when show-buffer
        (pop-to-buffer (llm--output-buffer root)))
      proc)))

;;; Interactive Commands

;;;###autoload
(defun llm-prompt-send ()
  "Send the contents of the prompt buffer to Claude."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (root llm--prompt-project-root))
    (when (string-empty-p prompt)
      (user-error "Empty prompt"))
    (kill-buffer (current-buffer))
    (llm--insert-prompt-header prompt root)
    (llm--start-process prompt nil t root)))

(defun llm-prompt-cancel ()
  "Cancel the prompt and close the prompt buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (message "Prompt cancelled"))

;;;###autoload
(defun llm-resume (session-id)
  "Resume a Claude session by SESSION-ID.
With prefix argument, prompt to edit the session ID."
  (interactive
   (let* ((root (llm--current-root))
          (current-sid (llm--project-get :session-id root)))
     (list (if (or current-prefix-arg (null current-sid))
               (read-string "Session ID: " current-sid)
             current-sid))))
  (let* ((root (llm--current-root))
         (prompt (read-string "Prompt: ")))
    (llm--insert-prompt-header (format "[resume %s] %s" session-id prompt) root)
    (llm--start-process prompt (list "--resume" session-id) t root)))

(defun llm--write-context-file (text &optional root)
  "Write TEXT to a temporary file inside the project root and return its path.
The file is placed under the project so the CLI has read access.
It is registered in the project state and deleted when the process exits."
  (let* ((r (or root (llm--project-root)))
         (dir (expand-file-name ".claude" r))
         (_ (make-directory dir t))
         (file (make-temp-file (expand-file-name "llm-context-" dir) nil ".txt")))
    (with-temp-file file (insert text))
    (llm--project-set :temp-files
                      (cons file (llm--project-get :temp-files r))
                      r)
    file))

(defun llm--send-to-claude (prompt)
  "Switch to the claude vterm buffer and insert PROMPT."
  (my/claude)
  (vterm-insert prompt))

;;;###autoload
(defun prompt ()
  "Smart Claude CLI dispatcher.  Behavior depends on context:
- C-u prefix: open multi-line prompt buffer
- Active region: send region as context with a prompt
- Otherwise: prompt at file+line (temp-file for non-file buffers)"
  (interactive)
  (let ((root (llm--current-root)))
    (cond
     ;; C-u → multi-line prompt buffer
     (current-prefix-arg
      (let ((buf (get-buffer-create "*llm-prompt*")))
        (with-current-buffer buf
          (llm-prompt-mode)
          (erase-buffer)
          (setq-local llm--prompt-project-root root))
        (pop-to-buffer buf)))
     ;; Active region → region context + prompt
     ((use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (prompt (read-string "Prompt: "))
             (file-name (or (buffer-file-name)
                            (llm--write-context-file
                             (buffer-substring-no-properties (point-min) (point-max))
                             root)))
             (start-line (line-number-at-pos start))
             (end-line (line-number-at-pos end))
             (context (buffer-substring-no-properties start end))
             (file (llm--write-context-file context root))
             (full-prompt (format "Read the file %s for context (from %s lines %d-%d), then answer: %s"
                                  file file-name start-line end-line prompt)))
        (deactivate-mark)
        (llm--send-to-claude full-prompt)))
     ;; Default → prompt at file+line
     (t
      (let* ((prompt (read-string "Prompt: "))
             (file-name (or (buffer-file-name)
                            (llm--write-context-file
                             (buffer-substring-no-properties (point-min) (point-max))
                             root)))
             (line (line-number-at-pos (point)))
             (full-prompt (format "File \"%s\", line %d: %s." file-name line prompt)))
        (llm--send-to-claude full-prompt))))))

;;;###autoload
(defun llm-select-model ()
  "Select the Claude model to use."
  (interactive)
  (setq llm-model (completing-read "Model: " '("sonnet" "opus" "haiku") nil t nil nil llm-model))
  (message "Model set to %s" llm-model))

;;;###autoload
(defun llm-stop ()
  "Stop the running Claude process for the current project."
  (interactive)
  (let* ((root (llm--current-root))
         (proc (llm--project-get :process root)))
    (if (and proc (process-live-p proc))
        (progn
          (kill-process proc)
          (llm--project-set :process nil root)
          (llm--insert-output "\n[stopped]\n" root)
          (message "Claude process stopped"))
      (message "No running process"))))

(defun llm-clear ()
  "Clear the output buffer for the current project."
  (interactive)
  (let ((root (llm--current-root)))
    (with-current-buffer (llm--output-buffer root)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "LLM output cleared")))

(defun llm-new-session ()
  "Forget the current session ID for this project, starting a fresh conversation.
The output buffer is not cleared; use `llm-clear' if you also want that."
  (interactive)
  (let ((root (llm--current-root)))
    (llm--project-set :session-id nil root)
    (message "LLM session reset — next prompt will start a new conversation")))

(defun llm-save-history ()
  "Manually save the output buffer to the project's llm-history.org file."
  (interactive)
  (let ((root (llm--current-root)))
    (llm--save-history root)
    (message "LLM history saved to %s" (llm--history-file root))))

(defun llm--last-response (&optional root)
  "Return the text of the last Claude response in ROOT's output buffer.
Signals `user-error' if no PROMPT heading is found.
Skips the blank line and optional :PROPERTIES:...:END: block that
`llm--insert-footer' may have inserted immediately after the heading."
  (let ((root (or root (llm--current-root))))
    (with-current-buffer (llm--output-buffer root)
      (save-excursion
        (let ((end (point-max)))
          (goto-char end)
          (if (re-search-backward
               (concat "^\\* " (regexp-quote llm--prompt-keyword) "\\b") nil t)
              (progn
                (forward-line 1)
                ;; Skip blank line after heading
                (when (looking-at-p "^$")
                  (forward-line 1))
                ;; Skip :PROPERTIES:...:END: block if present
                (when (looking-at-p "^:PROPERTIES:")
                  (re-search-forward "^:END:" nil t)
                  (forward-line 1))
                (string-trim (buffer-substring-no-properties (point) end)))
            (user-error "No response found")))))))

(defun llm-to-org ()
  "Insert the last Claude response as an org quote block at point."
  (interactive)
  (insert "#+begin_quote\n" (llm--last-response) "\n#+end_quote\n"))

(defun llm-copy-response ()
  "Copy the last Claude response to the kill ring."
  (interactive)
  (let ((text (llm--last-response)))
    (kill-new text)
    (message "Last response copied to kill ring (%d chars)" (length text))))

(defun llm-goto-output ()
  "Jump to the *llm* buffer named in a comment on the current line.
Looks for a pattern like [*llm:...*] in the current line."
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (if (string-match "\\[\\(\\*llm:[^*]+\\*\\)" line)
        (let ((buf-name (match-string 1 line)))
          (if (get-buffer buf-name)
              (pop-to-buffer buf-name)
            (user-error "Buffer %s does not exist" buf-name)))
      (user-error "No *llm:...* reference found on this line"))))

(defun llm-switch-buffer ()
  "Switch to an *llm:...* output buffer, with completion."
  (interactive)
  (let ((bufs (cl-remove-if-not
               (lambda (b) (string-match-p "\\`\\*llm:" (buffer-name b)))
               (buffer-list))))
    (unless bufs
      (user-error "No *llm:...* buffers exist"))
    (let* ((names (mapcar #'buffer-name bufs))
           (current-name (buffer-name))
           (current (when (string-match-p "\\`\\*llm:" current-name)
                      current-name))
           (default (or (car (cl-remove current names :test #'equal))
                        current))
           (choice (completing-read
                    (format-prompt "LLM buffer" default)
                    names nil t nil nil default)))
      (pop-to-buffer choice))))

;;; Change Highlighting on Revert

(defface llm-change-highlight-face
  '((((background dark))  :background "#1a3a1a" :extend t)
    (((background light)) :background "#d4f4d4" :extend t))
  "Face applied to lines added or changed in the last auto-revert.")

(defvar llm--pre-revert-contents (make-hash-table :test 'equal)
  "Hash-table mapping absolute file paths to their buffer text captured
just before `auto-revert-mode' reverts them.")

(defvar-local llm--change-highlight-timer nil
  "Buffer-local idle timer that removes `llm-change-highlight' overlays.")

(defun llm-change-highlight-clear (&optional buf)
  "Remove all change-highlight overlays from BUF (default: current buffer).
Also cancels the auto-clear timer if one is pending."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (when (timerp llm--change-highlight-timer)
      (cancel-timer llm--change-highlight-timer)
      (setq llm--change-highlight-timer nil))
    (remove-overlays (point-min) (point-max) 'category 'llm-change-highlight)))

(defun llm--before-revert-save ()
  "Hook: capture buffer text before `auto-revert-mode' reverts it."
  (when (buffer-file-name)
    (puthash (buffer-file-name)
             (buffer-substring-no-properties (point-min) (point-max))
             llm--pre-revert-contents)))

(defun llm--after-revert-highlight ()
  "Hook: highlight lines in the reverted buffer that differ from the snapshot."
  (when-let* ((file  (buffer-file-name))
              (old   (gethash file llm--pre-revert-contents)))
    (remhash file llm--pre-revert-contents)
    (let* ((new   (buffer-substring-no-properties (point-min) (point-max)))
           (lines (llm--diff-added-lines old new))
           (buf   (current-buffer)))
      (llm-change-highlight-clear buf)
      (save-excursion
        (dolist (lnum lines)
          (goto-char (point-min))
          (forward-line (1- lnum))
          (let ((ov (make-overlay (line-beginning-position)
                                  (min (point-max) (1+ (line-end-position))))))
            (overlay-put ov 'face     'llm-change-highlight-face)
            (overlay-put ov 'category 'llm-change-highlight)
            (overlay-put ov 'priority 10))))
      (setq llm--change-highlight-timer
            (run-with-timer 60 nil #'llm-change-highlight-clear buf)))))

(defun llm--diff-added-lines (old new)
  "Return a sorted list of 1-based line numbers added/changed in NEW vs OLD.
Delegates to the external `diff' binary using line-format options so only
the new-file line numbers of added lines are emitted — no diff parsing needed."
  (let ((old-file (make-temp-file "llm-diff-a"))
        (new-file (make-temp-file "llm-diff-b"))
        lines)
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old))
          (with-temp-file new-file (insert new))
          (with-temp-buffer
            (call-process "diff" nil t nil
                          "--new-line-format=%dn\n"
                          "--old-line-format="
                          "--unchanged-line-format="
                          old-file new-file)
            (goto-char (point-min))
            (while (re-search-forward "^\\([0-9]+\\)$" nil t)
              (push (string-to-number (match-string 1)) lines))))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file)))
    (nreverse lines)))

(add-hook 'before-revert-hook #'llm--before-revert-save)
(add-hook 'after-revert-hook  #'llm--after-revert-highlight)

;;; Keybindings

(defvar llm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'llm)
    (define-key map "r" #'llm-resume)
    (define-key map "m" #'llm-select-model)
    (define-key map "k" #'llm-stop)
    (define-key map "l" #'llm-clear)
    (define-key map "n" #'llm-new-session)
    (define-key map "o" #'llm-to-org)
    (define-key map "c" #'llm-copy-response)
    (define-key map "b" #'llm-switch-buffer)
    (define-key map "s" #'llm-save-history)
    (define-key map "g" #'llm-goto-output)
    (define-key map "h" #'llm-change-highlight-clear)
    map)
  "Keymap for LLM commands, bound under `C-x y a'.")

(global-set-key (kbd "C-x y a") llm-command-map)

(provide 'my-llm)
;;; my-llm.el ends here
