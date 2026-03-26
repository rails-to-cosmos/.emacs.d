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

(defvar llm--buffers (make-hash-table :test 'eq)
  "Hash table mapping claude vterm buffers to their status.")

(defun llm--register-buffer (buf)
  "Register BUF as a claude buffer with initial status nil."
  (puthash buf nil llm--buffers))

(defun llm--unregister-buffer (buf)
  "Unregister BUF from the claude buffer registry."
  (remhash buf llm--buffers))

(defun llm--get-buffers ()
  "Return a list of all live claude buffers."
  (cl-remove-if-not #'buffer-live-p (hash-table-keys llm--buffers)))

;;; Status Detection Patterns and Logic

(defvar llm--permission-pattern
  "Allow\\|allow\\|permit\\|approve\\|Yes.*No\\|\\(y\\).*\\(n\\)"
  "Regex matched against raw vterm output to detect permission prompts (→ blocked).")

(defvar llm--busy-pattern
  "esc to interrupt"
  "Regex to detect when Claude is actively working (→ busy).")

(defvar llm--user-input-pattern
  "^[^[:space:]].*[%$>#λ]\\s*$"
  "Regex to detect shell prompts and user input areas (ignored for status).")

(defun llm--status-from-output (buf input current-status)
  "Determine new status for BUF based on vterm OUTPUT.
Rules (in order):
1. If INPUT matches permission pattern → 'blocked'
2. If INPUT matches busy pattern → 'busy'
3. If INPUT is not a shell prompt and not 'busy' → 'busy'
4. Otherwise → keep current-status"
  (cond
   ((string-match-p llm--permission-pattern input) 'blocked)
   ((string-match-p llm--busy-pattern input) 'busy)
   ((not (string-match-p llm--user-input-pattern input))
    (unless (eq current-status 'busy) 'busy))
   (t current-status)))

(defun llm--last-terminal-line (buf)
  "Return the last non-empty line from the vterm terminal in BUF.
Uses `with-selected-window' avoidance to prevent scroll interference."
  (with-current-buffer buf
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (if (string-match "\\([^\n\r\t ][^\n]*\\)[\n\r\t ]*\\'" content)
          (match-string 1 content)
        ""))))

(defun llm--status-from-process (buf)
  "Determine new status for BUF based on process state and terminal content.
Rules:
1. If process is dead → 'exited'
2. If terminal still shows a permission prompt → 'blocked'
3. Otherwise → 'idle'"
  (let* ((proc vterm--process)
         (alive (and proc (process-live-p proc))))
    (cond ((not alive) 'exited)
          ((string-match-p llm--permission-pattern
                           (llm--last-terminal-line buf))
           'blocked)
          (t 'idle))))

(defun llm--project-label (directory)
  "Return (LABEL . ROOT) for the current project or directory."
  (let* ((proj (project-current nil directory))
         (root (when proj (project-root proj))))
    (cons (if root
              (file-name-nondirectory (directory-file-name root))
            (abbreviate-file-name directory))
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
  (pcase-let* ((`(,label . ,root) (llm--project-label (or user-root default-directory)))
               (default-directory (or user-root root default-directory))
               (base (format "*claude:%s*" label))
               (prefix (prefix-numeric-value current-prefix-arg)))
    (cond
     ((= prefix 1)
      (let ((existing (get-buffer base)))
        (if (and existing (buffer-live-p existing))
            (pop-to-buffer existing)
          (let ((vterm-shell (llm--claude-shell-command root)))
            (vterm-other-window base)
            (llm--register-buffer (current-buffer))))))
     ((= prefix 4)
      (let ((vterm-shell (llm--claude-shell-command root))
            (name (generate-new-buffer-name base)))
        (vterm-other-window name)
        (llm--register-buffer (current-buffer))))
     ((>= prefix 16)
      (let ((vterm-shell "claude")
            (name (generate-new-buffer-name base)))
        (vterm-other-window name)
        (llm--register-buffer (current-buffer)))))))

;;; Claude vterm status indicator

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
  "Update the status for BUF based on process state.
Uses `llm--status-from-process' to determine new status.
Only triggers a mode-line redraw when the status actually changes."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((old-status (gethash buf llm--buffers))
            (new-status (llm--status-from-process buf)))
        (unless (eq old-status new-status)
          (puthash buf new-status llm--buffers)
          (force-mode-line-update))))))

(defun llm--schedule-status-check ()
  "Schedule a debounced status check for the current claude buffer."
  (when (timerp llm--status-timer)
    (cancel-timer llm--status-timer))
  (setq llm--status-timer
        (run-with-timer 0.5 nil #'llm--detect-status (current-buffer))))

(defun llm--filter-advice (orig-fn process input)
  "After vterm processes output, update buffer status.
Uses `llm--status-from-output' to determine new status based on patterns.
Only redraws when the status actually changes to avoid vterm jitter."
  (funcall orig-fn process input)
  (when-let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (llm-buffer-p)
          (let* ((old-status (gethash buf llm--buffers))
                 (new-status (llm--status-from-output buf input old-status)))
            (when (and new-status (not (eq old-status new-status)))
              (puthash buf new-status llm--buffers)
              (force-mode-line-update))
            (llm--schedule-status-check)))))))

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
            (puthash (current-buffer) 'exited llm--buffers)
            (force-mode-line-update)))))))

(advice-add 'vterm--sentinel :around #'llm--sentinel-advice)

;; (defun llm--stable-redraw-advice (orig-fn buffer)
;;   "Preserve window scroll position for non-selected claude windows during redraw."
;;   (if (not (and (buffer-live-p buffer)
;;                 (with-current-buffer buffer (llm-buffer-p))))
;;       (funcall orig-fn buffer)
;;     ;; Save window-start and window-point for all non-selected windows showing this buffer.
;;     (let ((saved (cl-loop for win in (get-buffer-window-list buffer nil t)
;;                           unless (eq win (selected-window))
;;                           collect (list win
;;                                         (window-start win)
;;                                         (window-point win)))))
;;       (funcall orig-fn buffer)
;;       ;; Restore scroll position for non-selected windows.
;;       (dolist (entry saved)
;;         (let ((win (nth 0 entry))
;;               (start (nth 1 entry))
;;               (pt (nth 2 entry)))
;;           (when (window-live-p win)
;;             (set-window-start win start t)
;;             (set-window-point win pt)))))))

;; (advice-add 'vterm--delayed-redraw :around #'llm--stable-redraw-advice)

(defun llm--mode-line-status ()
  "Return a mode-line string showing the current claude buffer status.
Returns empty string for non-llm buffers."
  (if (not (llm-buffer-p))
      ""
    (let ((status (gethash (current-buffer) llm--buffers)))
      (pcase status
        ('idle    (propertize " ● idle" 'face 'llm-status-idle-face))
        ('busy    (propertize " ◉ busy" 'face 'llm-status-busy-face))
        ('blocked (propertize " ⊘ blocked" 'face 'llm-status-blocked-face))
        ('exited  (propertize " ○ exited" 'face 'llm-status-exited-face))
        (_        (propertize " ● idle" 'face 'llm-status-idle-face))))))

;; Clean up stale timers and unregister dead buffers on re-eval.
(when (hash-table-p llm--buffers)
  (maphash (lambda (buf _status)
             (unless (buffer-live-p buf)
               (remhash buf llm--buffers)))
           llm--buffers))
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (and (bound-and-true-p llm--status-timer)
               (timerp llm--status-timer))
      (cancel-timer llm--status-timer)
      (setq llm--status-timer nil))))

(defun llm--cleanup-buffer ()
  "Unregister the current buffer from the claude buffer list."
  (llm--unregister-buffer (current-buffer)))

(add-hook 'kill-buffer-hook #'llm--cleanup-buffer)

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
          (let ((status (gethash buf llm--buffers)))
            (if (memq status '(idle busy))
                (progn
                  (setq llm--prompt-queue (butlast llm--prompt-queue))
                  (vterm-insert prompt)
                  (vterm-send-return)
                  (when llm--prompt-queue
                    (run-with-timer 0.5 nil #'llm--drain-queue)))
              (run-with-timer 0.5 nil #'llm--drain-queue))))))))

(defun llm--save-prompt (prompt root)
  "Save PROMPT to .project/prompts/ in ROOT as a timestamped file."
  (let* ((r (or root (llm--current-root)))
         (dir (expand-file-name ".project/prompts" r))
         (file (expand-file-name
                (format "%s.txt" (format-time-string "%Y%m%d-%H%M%S"))
                dir)))
    (make-directory dir t)
    (with-temp-file file (insert prompt))))

(defun llm--send-to-claude (prompt &optional root)
  "Switch to the claude vterm buffer and insert PROMPT.
If ROOT is provided, switch to the claude buffer for that project root.
If the session is busy or blocked, queue the prompt and insert it
once the session becomes idle."
  (llm--save-prompt prompt root)
  (llm root)
  (let ((status (gethash (current-buffer) llm--buffers)))
    (if (memq status '(nil idle busy))
        (progn (vterm-insert prompt)
               (vterm-send-return))
      (let ((was-empty (null llm--prompt-queue)))
        (push (cons (current-buffer) prompt) llm--prompt-queue)
        (when was-empty
          (run-with-timer 0.5 nil #'llm--drain-queue))
        (message "Claude is %s — prompt queued (%d pending), will send when idle"
                 status (length llm--prompt-queue))))))

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
                  (file-name
                   (format "File \"%s\", line %d:\n\n"
                           file-name (line-number-at-pos (point))))))
         (buf (get-buffer-create "*llm-prompt*")))
    (with-current-buffer buf
      (llm-prompt-mode)
      (erase-buffer)
      (when prefix (insert prefix))
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
  (let ((bufs (llm--get-buffers)))
    (unless bufs
      (user-error "No claude buffers"))
    (let ((entries (cl-loop for buffer in bufs
                            collect (let ((name (buffer-name buffer))
                                          (status (gethash buffer llm--buffers)))
                                      (cons (if status
                                                (format "%s [%s]" name status)
                                              name)
                                            buffer)))))
      (let* ((choice (completing-read "Claude buffer: "
                                      (mapcar #'car entries)
                                      nil t))
             (buf (cdr (assoc choice entries))))
        (pop-to-buffer buf)))))

;;; FIXME/TODO Annotation System

(defvar llm--annotations (make-hash-table :test 'equal)
  "Hash table mapping (ROOT . KIND) to list of annotation entries.
KIND is a string like \"FIXME\" or \"TODO\".
Each entry is a plist (:file :line :text :time).")

(defun llm--annotation-file (root kind)
  "Return the persistence file path for KIND annotations in ROOT."
  (expand-file-name (format ".project/%s.el" (downcase kind)) root))

(defun llm--annotation-key (root kind)
  "Return the hash key for ROOT and KIND."
  (cons root kind))

(defun llm--annotation-load (root kind)
  "Load annotations of KIND for ROOT from disk."
  (let ((file (llm--annotation-file root kind)))
    (puthash (llm--annotation-key root kind)
             (when (file-readable-p file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (read (current-buffer))))
             llm--annotations)))

(defun llm--annotation-save (root kind)
  "Save annotations of KIND for ROOT to disk."
  (let ((file (llm--annotation-file root kind))
        (entries (gethash (llm--annotation-key root kind) llm--annotations)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (pp entries (current-buffer)))))

(defun llm--annotation-alive-p (entry kind)
  "Return non-nil if ENTRY's KIND(llm) comment still exists in the file."
  (let ((file (plist-get entry :file))
        (text (plist-get entry :text)))
    (and (file-readable-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (let ((needle (concat kind "(llm): " (car (split-string text "\n")))))
             (search-forward needle nil t))))))

(defun llm--annotation-entries (root kind)
  "Return the list of live KIND annotations for ROOT.
Loads from disk if needed, then prunes entries whose comment
has been removed from the source file."
  (let ((key (llm--annotation-key root kind)))
    (unless (gethash key llm--annotations)
      (llm--annotation-load root kind))
    (let* ((entries (gethash key llm--annotations))
           (live (cl-remove-if-not (lambda (e) (llm--annotation-alive-p e kind)) entries)))
      (unless (= (length entries) (length live))
        (puthash key live llm--annotations)
        (llm--annotation-save root kind))
      live)))

(defun llm--annotation-comment (kind text)
  "Return a KIND comment for TEXT using the current mode's comment syntax."
  (let ((cs (string-trim-right (or comment-start "# ")))
        (ce (let ((e (or comment-end ""))) (if (string-empty-p e) "" (concat " " e)))))
    (mapconcat (lambda (line)
                 (concat cs " " kind "(llm): " line ce))
               (split-string text "\n")
               "\n")))

(defvar-local llm--annotation-kind nil
  "The annotation kind (\"FIXME\" or \"TODO\") for the current prompt buffer.")
(defvar-local llm--annotation-source-buf nil)
(defvar-local llm--annotation-source-file nil)
(defvar-local llm--annotation-source-line nil)

(defun llm--annotation-send ()
  "Insert the annotation comment at the source location and save."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (kind llm--annotation-kind)
        (root llm--prompt-project-root)
        (source-buf llm--annotation-source-buf)
        (source-file llm--annotation-source-file)
        (source-line llm--annotation-source-line))
    (when (string-empty-p text)
      (user-error "Empty %s text" kind))
    (kill-buffer (current-buffer))
    (when (buffer-live-p source-buf)
      (with-current-buffer source-buf
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- source-line))
          (beginning-of-line)
          (open-line 1)
          (insert (llm--annotation-comment kind text))
          (indent-region (line-beginning-position) (line-end-position)))))
    (let* ((key (llm--annotation-key root kind))
           (entries (llm--annotation-entries root kind)))
      (push (list :file source-file :line source-line
                  :text text :time (format-time-string "%Y-%m-%d %H:%M"))
            entries)
      (puthash key entries llm--annotations))
    (llm--annotation-save root kind)
    (message "%s added at %s:%d" kind source-file source-line)))

(defun llm--add-annotation (kind)
  "Open a prompt buffer to compose a KIND annotation."
  (let* ((root (llm--current-root))
         (source-buf (current-buffer))
         (source-file (or (buffer-file-name) (buffer-name)))
         (source-line (line-number-at-pos (point)))
         (buf (get-buffer-create (format "*llm-%s*" (downcase kind)))))
    (with-current-buffer buf
      (llm-prompt-mode)
      (erase-buffer)
      (setq-local llm--prompt-project-root root)
      (setq-local llm--annotation-kind kind)
      (setq-local llm--annotation-source-buf source-buf)
      (setq-local llm--annotation-source-file source-file)
      (setq-local llm--annotation-source-line source-line)
      (setq header-line-format
            (format " %s  C-c C-c insert | C-c C-k cancel" kind))
      (use-local-map (let ((map (make-sparse-keymap)))
                        (set-keymap-parent map text-mode-map)
                        (define-key map (kbd "C-c C-c") #'llm--annotation-send)
                        (define-key map (kbd "C-c C-k") #'llm-prompt-cancel)
                        map)))
    (pop-to-buffer buf)))

(defun llm--list-annotations (kind)
  "List annotations of KIND for the current project with completion."
  (let* ((root (llm--current-root))
         (entries (llm--annotation-entries root kind)))
    (unless entries
      (user-error "No %ss in this project" kind))
    (let* ((candidates
            (mapcar (lambda (e)
                      (cons (format "%s:%d — %s [%s]"
                                    (file-relative-name (plist-get e :file) root)
                                    (plist-get e :line)
                                    (truncate-string-to-width (plist-get e :text) 60 nil nil "…")
                                    (plist-get e :time))
                            e))
                    entries))
           (choice (completing-read (format "%s: " kind)
                                    (mapcar #'car candidates) nil t))
           (entry (cdr (assoc choice candidates)))
           (file (plist-get entry :file)))
      (when (file-exists-p file)
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- (plist-get entry :line)))))))

(defun llm--send-annotations (kind)
  "Send all KIND annotations for the current project to Claude."
  (let* ((root (llm--current-root))
         (entries (llm--annotation-entries root kind)))
    (unless entries
      (user-error "No %ss in this project" kind))
    (let ((prompt (mapconcat
                   (lambda (e)
                     (format "%s at %s:%d — %s"
                             kind
                             (plist-get e :file)
                             (plist-get e :line)
                             (plist-get e :text)))
                   entries "\n")))
      (llm--send-to-claude
       (format "Resolve the following %ss in this project:\n\n%s" kind prompt)))))

;;;###autoload
(defun llm-add-fixme ()
  "Add a FIXME annotation at point."
  (interactive)
  (llm--add-annotation "FIXME"))

;;;###autoload
(defun llm-add-todo ()
  "Add a TODO annotation at point."
  (interactive)
  (llm--add-annotation "TODO"))

;;;###autoload
(defun llm-list-fixmes ()
  "List all FIXMEs for the current project."
  (interactive)
  (llm--list-annotations "FIXME"))

;;;###autoload
(defun llm-list-todos ()
  "List all TODOs for the current project."
  (interactive)
  (llm--list-annotations "TODO"))

;;;###autoload
(defun llm-send-fixmes ()
  "Send all FIXMEs to Claude."
  (interactive)
  (llm--send-annotations "FIXME"))

;;;###autoload
(defun llm-send-todos ()
  "Send all TODOs to Claude."
  (interactive)
  (llm--send-annotations "TODO"))

;;;###autoload
(defun llm-grep-annotations ()
  "Grep all TODO/FIXME/HACK/XXX comments in the current project."
  (interactive)
  (let ((root (llm--current-root)))
    (grep-find (format "grep -rnE '(TODO|FIXME|HACK|XXX):?' %s --include='*.*' -I"
                       (shell-quote-argument (directory-file-name root))))))

;;; Keybindings

(global-set-key (kbd "C-x y e p") #'llm-prompt)
(global-set-key (kbd "C-x y e c") #'llm)
(global-set-key (kbd "C-x y e b") #'llm-switch-buffer)
(global-set-key (kbd "C-x y e h") #'llm-change-highlight-clear)
(global-set-key (kbd "C-x y e f") #'llm-add-fixme)
(global-set-key (kbd "C-x y e t") #'llm-add-todo)
(global-set-key (kbd "C-x y e F") #'llm-list-fixmes)
(global-set-key (kbd "C-x y e T") #'llm-list-todos)
(global-set-key (kbd "C-x y e S") #'llm-send-fixmes)
(global-set-key (kbd "C-x y e G") #'llm-grep-annotations)

(provide 'mijn-llm)
;;; my-llm.el ends here
