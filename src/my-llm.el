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
(defun my/claude (&optional user-root)
  "Open Claude CLI in a vterm buffer named *claude:project*.
Without prefix: reuse the existing buffer, or create one.
With \\[universal-argument]: new buffer, continue session if possible.
With \\[universal-argument] \\[universal-argument]: new buffer, fresh session."
  (interactive)
  (pcase-let* ((`(,label . ,root) (llm--project-label))
               (default-directory (or user-root root default-directory))
               (base (format "*claude:%s*" label))
               (prefix (prefix-numeric-value current-prefix-arg)))
    (cond
     ((= prefix 1)
      (let ((existing (get-buffer base)))
        (if (and existing (buffer-live-p existing))
            (switch-to-buffer existing)
          (let ((vterm-shell (llm--claude-shell-command root)))
            (vterm base)))))
     ((= prefix 4)
      (let ((vterm-shell (llm--claude-shell-command root))
            (name (generate-new-buffer-name base)))
        (vterm name)))
     ((>= prefix 16)
      (let ((vterm-shell "claude")
            (name (generate-new-buffer-name base)))
        (vterm name))))))

;;; Claude vterm status indicator

(defvar-local my/claude-status nil
  "Status of a claude vterm buffer: nil, `input', `busy', or `exited'.")

(defvar-local my/claude--status-timer nil
  "Debounce timer for status detection in claude vterm buffers.")

(defface my/claude-status-input-face
  '((t :foreground "green3"))
  "Face for claude status when waiting for user input.")

(defface my/claude-status-busy-face
  '((t :foreground "dark orange"))
  "Face for claude status when thinking/working.")

(defface my/claude-status-exited-face
  '((t :foreground "gray50"))
  "Face for claude status when process has exited.")

(defun my/claude-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a claude vterm buffer."
  (string-prefix-p "*claude:" (buffer-name (or buf (current-buffer)))))

(defun my/claude--detect-status (buf)
  "Update `my/claude-status' in BUF based on output activity."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((proc vterm--process)
             (alive (and proc (process-live-p proc))))
        (setq my/claude-status (if alive 'input 'exited))
        (force-mode-line-update)))))

(defun my/claude--schedule-status-check ()
  "Schedule a debounced status check for the current claude buffer."
  (when (timerp my/claude--status-timer)
    (cancel-timer my/claude--status-timer))
  (setq my/claude--status-timer
        (run-with-timer 0.5 nil #'my/claude--detect-status (current-buffer))))

(defun my/claude--filter-advice (orig-fn process input)
  "After vterm processes output, schedule a status check for claude buffers."
  (funcall orig-fn process input)
  (when-let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (my/claude-buffer-p)
          (unless (eq my/claude-status 'busy)
            (setq my/claude-status 'busy)
            (force-mode-line-update))
          (my/claude--schedule-status-check))))))

(advice-add 'vterm--filter :around #'my/claude--filter-advice)

(defun my/claude--sentinel-advice (orig-fn process event)
  "Update claude status when the vterm process exits."
  (funcall orig-fn process event)
  (when (and (string-match-p "\\`\\(finished\\|exited\\|signal\\)" event)
             (not (process-live-p process)))
    (when-let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (my/claude-buffer-p)
            (when (timerp my/claude--status-timer)
              (cancel-timer my/claude--status-timer)
              (setq my/claude--status-timer nil))
            (setq my/claude-status 'exited)
            (force-mode-line-update)))))))

(advice-add 'vterm--sentinel :around #'my/claude--sentinel-advice)

(defun my/claude--mode-line-status ()
  "Return a mode-line string showing claude buffer status."
  (pcase my/claude-status
    ('input  (propertize " ● input" 'face 'my/claude-status-input-face))
    ('busy   (propertize " ◉ busy" 'face 'my/claude-status-busy-face))
    ('exited (propertize " ○ exited" 'face 'my/claude-status-exited-face))))

;; Clean up stale timers in claude buffers on re-eval.
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (and (bound-and-true-p my/claude--status-timer)
               (timerp my/claude--status-timer))
      (cancel-timer my/claude--status-timer)
      (setq my/claude--status-timer nil))))

(let ((entry '(:eval (my/claude--mode-line-status))))
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

(defun llm--send-to-claude (prompt)
  "Switch to the claude vterm buffer and insert PROMPT."
  (my/claude)
  (vterm-insert prompt))

(defun llm--write-context-file (text)
  "Write TEXT to a temporary file and return its path."
  (let ((file (make-temp-file "llm-context-" nil ".txt")))
    (with-temp-file file (insert text))
    file))

;;;###autoload
(defun llm-prompt-send ()
  "Send the contents of the prompt buffer to Claude."
  (interactive)
  (let ((prompt (string-trim (buffer-string))))
    (when (string-empty-p prompt)
      (user-error "Empty prompt"))
    (kill-buffer (current-buffer))
    (llm--send-to-claude prompt)))

(defun llm-prompt-cancel ()
  "Cancel the prompt and close the prompt buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (message "Prompt cancelled"))

;;;###autoload
(defun llm-prompt ()
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
                             (buffer-substring-no-properties (point-min) (point-max)))))
             (start-line (line-number-at-pos start))
             (end-line (line-number-at-pos end))
             (context (buffer-substring-no-properties start end))
             (file (llm--write-context-file context))
             (full-prompt (format "Read the file %s for context (from %s lines %d-%d), then answer: %s"
                                  file file-name start-line end-line prompt)))
        (deactivate-mark)
        (llm--send-to-claude full-prompt)))
     ;; Default → prompt at file+line
     (t
      (let* ((prompt (read-string "Prompt: "))
             (file-name (or (buffer-file-name)
                            (llm--write-context-file
                             (buffer-substring-no-properties (point-min) (point-max)))))
             (line (line-number-at-pos (point)))
             (suffix (if (string-match-p "[.!?]\\'" prompt) "" "."))
             (full-prompt (format "File \"%s\", line %d: %s%s" file-name line prompt suffix)))
        (llm--send-to-claude full-prompt))))))

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

;;; Keybindings

(global-set-key (kbd "C-x y e p") #'llm-prompt)
(global-set-key (kbd "C-x y e c") #'my/claude)
(global-set-key (kbd "C-x y e h") #'llm-change-highlight-clear)

(provide 'my-llm)
;;; my-llm.el ends here
