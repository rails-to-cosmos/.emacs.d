(require 'eshell)
(require 'esh-mode)
(require 'init-org)
(require 'direnv)

(ob-add-language 'shell (cons "shell" "src shell"))
(ob-add-language 'eshell (cons "eshell" "src eshell"))

(defun my/--project-label ()
  "Return a short label for the current project or directory."
  (let* ((proj (project-current nil default-directory))
         (root (when proj (project-root proj))))
    (cons (if root
              (file-name-nondirectory (directory-file-name root))
            (abbreviate-file-name default-directory))
          root)))

(defun my/--claude-shell-command (root)
  "Return the claude shell command, using `-c' if ROOT has a `.claude/' dir."
  (if (and root (file-directory-p (expand-file-name ".claude" root)))
      "claude -c"
    "claude"))

(defun my/vterm ()
  "Open a vterm buffer named after the project or directory.
Appends <N> to resolve name collisions."
  (interactive)
  (let* ((label (car (my/--project-label)))
         (base (format "*vterm:%s*" label))
         (name (generate-new-buffer-name base)))
    (vterm name)))

(defun my/claude ()
  "Open Claude CLI in a vterm buffer named *claude:project*.
Without prefix: reuse the existing *claude:project* buffer, or create one
with `claude -c' if `.claude/' exists, plain `claude' otherwise.
With \\[universal-argument]: open a new vterm claude buffer (uses
`claude -c' when `.claude/' exists).
With \\[universal-argument] \\[universal-argument]: open a new vterm
claude buffer with a fresh session (plain `claude')."
  (interactive)
  (pcase-let* ((`(,label . ,root) (my/--project-label))
               (default-directory (or root default-directory))
               (base (format "*claude:%s*" label))
               (prefix (prefix-numeric-value current-prefix-arg)))
    (cond
     ;; No prefix: reuse existing buffer if alive
     ((= prefix 1)
      (let ((existing (get-buffer base)))
        (if (and existing (buffer-live-p existing))
            (switch-to-buffer existing)
          (let ((vterm-shell (my/--claude-shell-command root)))
            (vterm base)))))
     ;; C-u: new buffer, continue session if possible
     ((= prefix 4)
      (let ((vterm-shell (my/--claude-shell-command root))
            (name (generate-new-buffer-name base)))
        (vterm name)))
     ;; C-u C-u: new buffer, fresh session
     ((>= prefix 16)
      (let ((vterm-shell "claude")
            (name (generate-new-buffer-name base)))
        (vterm name))))))

(defun my/vterm-switch-buffer ()
  "Switch between vterm buffers using `switch-to-buffer'."
  (interactive)
  (let ((vterm-bufs (cl-remove-if-not
                     (lambda (b) (with-current-buffer b (derived-mode-p 'vterm-mode)))
                     (buffer-list))))
    (unless vterm-bufs
      (user-error "No vterm buffers"))
    (switch-to-buffer
     (completing-read "Vterm buffer: "
                      (mapcar #'buffer-name vterm-bufs)
                      nil t))))

(use-package vterm
  :bind (("C-x y e e" . my/vterm)
         ("C-x y e b" . my/vterm-switch-buffer)
         ("C-x y e c" . my/claude))
  :ensure t)

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
  (when-let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (my/claude-buffer-p)
          (setq my/claude-status 'exited)
          (force-mode-line-update))))))

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

(defun my/eshell-apply-dir-locals ()
  "Apply .dir-locals.el settings to the current buffer.
Handles regular file buffers and Eshell buffers correctly."
  (interactive)
  (hack-dir-local-variables)
  (cl-loop for (key . val) in file-local-variables-alist
           do (insert (if (eq key 'eval)
                          (prin1-to-string val)
                        (format "(setq-local %s (quote %s))" key val)))
           (eshell-send-input)))

(add-hook 'eshell-mode-hook #'my/eshell-apply-dir-locals)
(add-hook 'eshell-mode-hook #'direnv)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :ensure t)

(use-package eshell-prompt-extras
  :ensure t)

(defun eshell-compl-space ()
  (interactive)
  (cond
   ((looking-back "cd" 1)
    (condition-case exc
        (progn
          (insert " ")
          (sit-for 0.05)
          (progn
            (insert (shell-quote-argument (read-directory-name "Choose directory: ")))
            (eshell-send-input)
            (insert "ls -la")
            (eshell-send-input)))
      ('quit
       (eshell-kill-input))))
   ((looking-back "\\./" 1)
    (condition-case exc
        (progn
          (sit-for 0.05)
          (insert (shell-quote-argument (file-relative-name (read-file-name "Choose file or directory: ")))))
      ('quit )))
   (t (insert " "))))

(defun eshell-compl-back ()
  (interactive)
  (cond
   ((or (equal (point-marker) eshell-last-input-start)
        (looking-back "λ " 1))
    (setq eshell-last-input-start (point-marker)))
   (t (delete-backward-char 1))))

(with-eval-after-load 'em-alias
    ;;; TODO: This conflicts with `evil-define-key' during the initialization of
    ;;; the first eshell session: the map in insert-mode will not take the changes
    ;;; into account. Going to normal mode and back to insert mode works.
  (eshell-read-aliases-list)
  (dolist
      (alias
       '(("l" "ls -l $*")
         ("la" "ls -lAh $*")
         ("ll" "ls -lh $*")
         ("cp" "*cp -i $*")
         ("mv" "*mv -i $*")
         ("mkdir" "*mkdir -p $*")
         ("mkcd" "*mkdir -p $* && cd $1")))
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

(autoload 'epe-theme-lambda "eshell-prompt-extras")

(defun eshell-set-keys ()
  (define-key eshell-mode-map (kbd "<SPC>") 'eshell-compl-space)
  (define-key eshell-mode-map (kbd "<backspace>") 'eshell-compl-back))

(add-hook 'eshell-first-time-mode-hook 'eshell-set-keys)

(require 'ol)

(defun eshell-browse-directory (dir)
  (interactive "sDirectory: ")
  (let ((eshell-buffer-name (format "*eshell:%s*" (file-name-directory dir))))
    (eshell)
    (insert (format "cd %s" dir))
    (eshell-send-input)
    (insert "ls -la")
    (eshell-send-input)))

(org-link-set-parameters
 "eshell"
 :follow #'eshell-browse-directory
 ;; :face 'org-glance-link-materialize-face
 ;; :complete 'org-glance-link:choose-thing-for-materialization
 ;; :export #'org-glance-link:export
 ;; :store #'org-glance-link:store-link
 )

(use-package term
  :ensure nil ;; Built-in package
  :config (progn
            ;; (setq explicit-shell-file-name (or (getenv "SHELL") "/bin/bash"))
            (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
            (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
            (define-key term-raw-map (kbd "M-:") 'eval-expression)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "C-x") nil)
            (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
            (define-key term-mode-map (kbd "C-c C-j") 'term-char-mode))

  :hook (term-mode . (lambda ()
                       ;; (term-char-mode)
                       ;; (display-line-numbers-mode 0)
                       (setq-local term-buffer-maximum-size 10000))))

(provide 'init-terminal)
