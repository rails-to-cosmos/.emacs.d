(require 'eshell)
(require 'esh-mode)
(require 'my-org)
(require 'direnv)

(ob-add-language 'shell (cons "shell" "src shell"))
(ob-add-language 'eshell (cons "eshell" "src eshell"))

(defun my/vterm ()
  "Open or switch to a vterm buffer.
Without prefix: switch to the last vterm buffer, or create one if none exist.
With prefix: create a new vterm buffer named after the project or directory."
  (interactive)
  (let* ((proj (project-current nil default-directory))
         (root (when proj (project-root proj)))
         (label (if root
                    (file-name-nondirectory (directory-file-name root))
                  (abbreviate-file-name default-directory)))
         (base (format "*vterm:%s*" label))
         (name (generate-new-buffer-name base)))
    (if current-prefix-arg
        ;; With prefix: create new vterm
        (vterm name)

      ;; Without prefix: reuse or create
      (let ((vterm-bufs (cl-remove-if-not
                         (lambda (b) (string-prefix-p (format "*vterm:%s*" label) (buffer-name b)))
                         (buffer-list))))
        (if vterm-bufs
            (switch-to-buffer (car vterm-bufs))
          ;; No vterm buffers exist, create one
          (let* ((proj (project-current nil default-directory))
                 (root (when proj (project-root proj)))
                 (label (if root
                            (file-name-nondirectory (directory-file-name root))
                          (abbreviate-file-name default-directory)))
                 (base (format "*vterm:%s*" label))
                 (name (generate-new-buffer-name base)))
            (vterm name))))))
  )

(use-package vterm
  :bind (("C-x y e v" . my/vterm)
         ("C-x y e e" . eshell))
  :ensure t)

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
 :follow #'eshell-browse-directory)

(use-package term
  :ensure nil ;; Built-in package
  :config (progn
            (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
            (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
            (define-key term-raw-map (kbd "M-:") 'eval-expression)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "C-x") nil)
            (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
            (define-key term-mode-map (kbd "C-c C-j") 'term-char-mode))

  :hook (term-mode . (lambda ()
                       (setq-local term-buffer-maximum-size 10000))))

(provide 'my-terminal)
