;;; my-llm.el --- Claude CLI integration for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar vterm-shell)

;;; Project Root Detection

(defvar llm--project-root-markers '(".git" ".claude" "CLAUDE.md")
  "Files/dirs that indicate a project root.")

(cl-defun llm--project-root (&optional (dir default-directory))
  "Find project root starting from DIR by looking for marker files."
  (or (cl-loop for marker in llm--project-root-markers
               for root = (locate-dominating-file dir marker)
               when root return (file-name-as-directory root))
      (file-name-as-directory dir)))

(defvar-local llm--prompt-project-root nil
  "Project root captured when the prompt buffer was opened.")

(defun llm--current-root ()
  "Get the project root for the current context."
  (or llm--prompt-project-root
      (llm--project-root)))

;;; Claude vterm buffer management

(defun llm--project-label ()
  "Return (LABEL . ROOT) for the current project or directory."
  (let* ((proj (project-current nil default-directory))
         (root (when proj (project-root proj))))
    (cons (if root
              (file-name-nondirectory (directory-file-name root))
            (abbreviate-file-name default-directory))
          root)))

(defun llm--claude-shell-command (root)
  "Return the claude shell command, using `-c' if ROOT has a `.claude/' dir."
  (if (and root (file-directory-p (expand-file-name ".claude" root)))
      "claude -c"
    "claude"))

;;;###autoload
(defun llm (&optional user-root)
  "Open Claude CLI in a vterm buffer named *claude:project*.
Without prefix: reuse the existing buffer, or create one.
With \\[universal-argument]: new buffer, continue session if possible.
With \\[universal-argument] \\[universal-argument]: new buffer, fresh session."
  (interactive)
  (pcase-let* ((default-directory (or user-root root default-directory))
               (`(,label . ,root) (llm--project-label))
               (base (format "*claude:%s*" label))
               (prefix (prefix-numeric-value current-prefix-arg)))
    (cond
     ((= prefix 1)
      (let ((existing (get-buffer base)))
        (if (and existing (buffer-live-p existing))
            (pop-to-buffer existing)
          (let ((vterm-shell (llm--claude-shell-command root)))
            (vterm-other-window base)))))
     ((= prefix 4)
      (let ((vterm-shell (llm--claude-shell-command root))
            (name (generate-new-buffer-name base)))
        (vterm-other-window name)))
     ((>= prefix 16)
      (let ((vterm-shell "claude")
            (name (generate-new-buffer-name base)))
        (vterm-other-window name))))))

;;; Claude vterm status indicator

(defvar-local llm-status nil
  "Status of a claude vterm buffer: nil, `input', `busy', or `exited'.")

(defvar-local llm--status-timer nil
  "Debounce timer for status detection in claude vterm buffers.")

(defface llm-status-idle-face
  '((t :foreground "green3"))
  "Face for claude status when idle/waiting for user input.")

(defface llm-status-busy-face
  '((t :foreground "dark orange"))
  "Face for claude status when thinking/working.")

(defface llm-status-blocked-face
  '((t :foreground "red"))
  "Face for claude status when waiting for user approval.")

(defface llm-status-exited-face
  '((t :foreground "gray50"))
  "Face for claude status when process has exited.")

(defun llm-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a claude vterm buffer."
  (string-prefix-p "*claude:" (buffer-name (or buf (current-buffer)))))

(defun llm--detect-status (buf)
  "Update `llm-status' in BUF based on output activity.
Preserves `blocked' status; otherwise sets `input' or `exited'."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((proc vterm--process)
             (alive (and proc (process-live-p proc))))
        (setq llm-status
              (cond ((not alive) 'exited)
                    ((eq llm-status 'blocked) 'blocked)
                    (t 'idle)))
        (force-mode-line-update)))))

(defun llm--schedule-status-check ()
  "Schedule a debounced status check for the current claude buffer."
  (when (timerp llm--status-timer)
    (cancel-timer llm--status-timer))
  (setq llm--status-timer
        (run-with-timer 0.5 nil #'llm--detect-status (current-buffer))))

(defvar llm--permission-pattern
  "Allow\\|allow\\|permit\\|approve\\|Yes.*No\\|\\(y\\).*\\(n\\)"
  "Regex matched against raw vterm output to detect permission prompts.")

(defvar llm--busy-pattern
  "press esc to interrupt"
  "Regex to detect when Claude is actively working (busy).")

(defvar llm--user-input-pattern
  "^[^[:space:]].*[%$>#λ]\\s*$"
  "Regex to detect shell prompts and user input areas.
Excludes these from busy status detection.")

(defun llm--filter-advice (orig-fn process input)
  "After vterm processes output, schedule a status check for claude buffers.
Detects permission prompts in raw output to set `blocked' status.
Detects busy indicator and ignores user input areas."
  (funcall orig-fn process input)
  (when-let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (llm-buffer-p)
          (cond
           ((string-match-p llm--permission-pattern input)
            (setq llm-status 'blocked))
           ((string-match-p llm--busy-pattern input)
            (setq llm-status 'busy))
           ;; Only set busy if output is not just a shell prompt/input area
           ((not (string-match-p llm--user-input-pattern input))
            (unless (eq llm-status 'busy)
              (setq llm-status 'busy))))
          (force-mode-line-update)
          (llm--schedule-status-check))))))

(advice-add 'vterm--filter :around #'llm--filter-advice)

(defun llm--sentinel-advice (orig-fn process event)
  "Update claude status when the vterm process exits."
  (funcall orig-fn process event)
  (when (and (string-match-p "\\`\\(finished\\|exited\\|signal\\)" event)
             (not (process-live-p process)))
    (when-let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (llm-buffer-p)
            (when (timerp llm--status-timer)
              (cancel-timer llm--status-timer)
              (setq llm--status-timer nil))
            (setq llm-status 'exited)
            (force-mode-line-update)))))))

(advice-add 'vterm--sentinel :around #'llm--sentinel-advice)

(defun llm--mode-line-status ()
  "Return a mode-line string showing claude buffer status."
  (pcase llm-status
    ('idle   (propertize " ● idle" 'face 'llm-status-idle-face))
    ('busy    (propertize " ◉ busy" 'face 'llm-status-busy-face))
    ('blocked (propertize " ⊘ blocked" 'face 'llm-status-blocked-face))
    ('exited  (propertize " ○ exited" 'face 'llm-status-exited-face))))

;; Clean up stale timers in claude buffers on re-eval.
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (and (bound-and-true-p llm--status-timer)
               (timerp llm--status-timer))
      (cancel-timer llm--status-timer)
      (setq llm--status-timer nil))))

(let ((entry '(:eval (llm--mode-line-status))))
  (setq-default mode-line-misc-info
                (cons entry
                      (cl-remove entry (default-value 'mode-line-misc-info)
                                 :test #'equal))))

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
  (setq header-line-format " Claude  C-c C-c send | C-c C-k cancel"))

;;; Interactive Commands

(defvar llm--prompt-queue nil
  "LIFO queue of (BUFFER . PROMPT) entries waiting to be sent when claude is idle.")

(defun llm--drain-queue ()
  "Pop the next prompt from `llm--prompt-queue' and send it when idle."
  (when-let ((entry (car (last llm--prompt-queue))))
    (let ((buf (car entry))
          (prompt (cdr entry)))
      (if (not (buffer-live-p buf))
          (progn
            (setq llm--prompt-queue (butlast llm--prompt-queue))
            (llm--drain-queue))
        (with-current-buffer buf
          (if (eq llm-status 'idle)
              (progn
                (setq llm--prompt-queue (butlast llm--prompt-queue))
                (vterm-insert prompt)
                (when llm--prompt-queue
                  (run-with-timer 0.5 nil #'llm--drain-queue)))
            (run-with-timer 0.5 nil #'llm--drain-queue)))))))

(defun llm--send-to-claude (prompt &optional root)
  "Switch to the claude vterm buffer and insert PROMPT.
If ROOT is provided, switch to the claude buffer for that project root.
If the session is busy or blocked, queue the prompt and insert it
once the session becomes idle."
  (llm root)
  (if (memq llm-status '(nil idle))
      (vterm-insert prompt)
    (let ((was-empty (null llm--prompt-queue)))
      (push (cons (current-buffer) prompt) llm--prompt-queue)
      (when was-empty
        (run-with-timer 0.5 nil #'llm--drain-queue))
      (message "Claude is %s — prompt queued (%d pending), will send when idle"
               llm-status (length llm--prompt-queue)))))

(defun llm--write-context-file (text)
  "Write TEXT to a temporary file and return its path."
  (let ((file (make-temp-file "llm-context-" nil ".txt")))
    (with-temp-file file (insert text))
    file))

;;;###autoload
(defun llm-prompt-send ()
  "Send the contents of the prompt buffer to Claude.
Sends to the Claude buffer corresponding to the project root where
the prompt was opened."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (root llm--prompt-project-root))
    (when (string-empty-p prompt)
      (user-error "Empty prompt"))
    (kill-buffer (current-buffer))
    (llm--send-to-claude prompt root)))

(defun llm-prompt-cancel ()
  "Cancel the prompt and close the prompt buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (message "Prompt cancelled"))

;;;###autoload
(defun llm-prompt ()
  "Open a multi-line prompt buffer for Claude.
Pre-populates context based on the current state:
- Active region: inserts a file/region context prefix
- Otherwise: inserts a file+line context prefix"
  (interactive)
  (let* ((proj (project-current nil default-directory))
         (root (when proj (project-root proj)))
         (root (or root (llm--project-root default-directory)))
         (file-name (buffer-file-name))
         (prefix (cond
                  ((use-region-p)
                   (let* ((start (region-beginning))
                          (end (region-end))
                          (context (buffer-substring-no-properties start end))
                          (file (if file-name
                                    file-name
                                  (llm--write-context-file context))))
                     (deactivate-mark)
                     (if file-name
                         (format "Context: %s from lines %d-%d\n\n"
                                 file (line-number-at-pos start) (line-number-at-pos end))
                       (format "Context: %s\n\n" file))))
                  (t
                   (let ((file (if file-name
                                   file-name
                                 (llm--write-context-file
                                  (buffer-substring-no-properties (point-min) (point-max))))))
                     (format "File \"%s\", line %d:\n\n"
                             file (line-number-at-pos (point)))))))
         (buf (get-buffer-create "*llm-prompt*")))
    (with-current-buffer buf
      (llm-prompt-mode)
      (erase-buffer)
      (insert prefix)
      (setq-local llm--prompt-project-root root))
    (pop-to-buffer buf)))

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
  "Return a sorted list of 1-based line numbers added/changed in NEW vs OLD."
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

;;;###autoload
(defun llm-switch-buffer ()
  "Switch between claude buffers, showing status in the menu."
  (interactive)
  (let ((bufs (cl-remove-if-not
               (lambda (b) (and (with-current-buffer b (derived-mode-p 'vterm-mode))
                                (llm-buffer-p b)))
               (buffer-list))))
    (unless bufs
      (user-error "No claude buffers"))
    (let* ((entries (mapcar (lambda (b)
                              (let ((status (buffer-local-value 'llm-status b)))
                                (cons (if status
                                          (format "%s [%s]" (buffer-name b) status)
                                        (buffer-name b))
                                      b)))
                            bufs))
           (choice (completing-read "Claude buffer: "
                                    (mapcar #'car entries)
                                    nil t))
           (buf (cdr (assoc choice entries))))
      (pop-to-buffer buf))))

;;; Keybindings

(global-set-key (kbd "C-x y e p") #'llm-prompt)
(global-set-key (kbd "C-x y e c") #'llm)
(global-set-key (kbd "C-x y e b") #'llm-switch-buffer)
(global-set-key (kbd "C-x y e h") #'llm-change-highlight-clear)

(provide 'mijn-llm)
;;; my-llm.el ends here
