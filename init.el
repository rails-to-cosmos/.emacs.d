;;; init.el --- my emacs configuration
;;; Commentary:
;;; Code:

(require 'package)

(dolist (package-archive
         '(("melpa" . "http://melpa.milkbox.net/packages/")
           ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
           ("elpy" . "https://jorgenschaefer.github.io/packages/")))
  (add-to-list 'package-archives package-archive))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package simplifies
  :load-path "packages/core")

(use-package init-req-dirs
  :load-path "packages/core"
  :commands (tmp/ dropbox/))

(use-package init-mac
  :load-path "packages/core")

(use-package init-bindings
  :load-path "packages/core")

(use-package init-shell
  :load-path "packages/core")

(use-package init-windows
  :load-path "packages/windows"
  :init (progn
          (add-hook 'after-init-hook 'prevent-active-processes-exist))
  :bind (("C-x o" . switch-window)
         ("C-x 1" . delete-other-windows)
         ("C-x 2" . split-window-vertically-swap)
         ("C-x 3" . split-window-horizontally-swap))
  :commands (prevent-active-processes-exist
             split-window-func-with-other-buffer
             immortal-scratch
             delete-this-file
             rename-this-file-and-buffer
             save-buffers-kill-emacs
             get-window-in-frame
             set-window-buffer-in-frame))

(use-package wgrep
  :ensure t)

(use-package flycheck
  :config (progn
            (setq-default
             flycheck-check-syntax-automatically '(save idle-change mode-enabled)
             flycheck-idle-change-delay 5
             flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
            (global-flycheck-mode))
   :ensure t)

(use-package snippets
  :load-path "packages/editor"
  :config (progn
            (add-to-list 'yas-snippet-dirs (dropbox/ "snippets")))
  :commands (yas-ido-expand
             yas-new-snippet
             yas-recompile-all
             yas-reload-all))

(use-package macro
  :load-path "packages/editor"
  :commands (save-macro))

(use-package company
  :config (progn
            (global-company-mode))
  :ensure t)

(use-package hippie-expand
  :bind ("M-/" . hippie-expand)
  :config (progn
            (message "Hello there, hippies!")
            (setq-default hippie-expand-try-functions-list
                          '(try-expand-dabbrev
                            try-expand-dabbrev-all-buffers
                            try-expand-dabbrev-from-kill
                            try-complete-file-name-partially
                            try-complete-file-name
                            try-expand-all-abbrevs
                            try-expand-list
                            try-expand-line
                            try-complete-lisp-symbol-partially
                            try-complete-lisp-symbol))))

(use-package folding
  :load-path "packages/editor"
  :bind (:map prog-mode-map
              ("C-c f t" . origami-toggle-node)
              ("C-c f r" . origami-recursively-toggle-node)
              ("C-c f o" . origami-show-only-node)))

(use-package log
  :load-path "packages/editor"
  :commands (mwe:log-keyboard-commands))

(use-package init-python
  :load-path "prog"

  :init (progn
          (autoload 'elpy-mode-map "elpy" "Define elpy-mode-map." t)
          (add-hook 'python-mode-hook 'init-python)
          (add-hook 'python-mode-hook 'python-highlight-breakpoints)
          (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
          (add-hook 'python-mode-hook 'linum-mode)
          (add-hook 'python-mode-hook 'elpy-mode))

  :mode ("\\.py\\'" . python-mode)

  :bind (:map elpy-mode-map
              ("C-c C-b" . python-add-breakpoint)
              ("C-c C-g" . jedi:goto-definition))

  :commands (init-python
             python-add-breakpoint
             python-highlight-breakpoints
             jedi:goto-definition
             elpy-mode))

(use-package init-db
  :load-path "prog"
  :config (progn
            (setq-default edbi:ds-history-file (tmp/ "edbi-ds-history.txt")
                          sqlplus-session-cache-dir (tmp/ "sqlplus-session")))
  :bind (("C-x y q" . sqp-connect)))

(use-package xmpp
  :load-path "chat"
  :config (progn
            (setq-default
             jabber-global-history-filename (dropbox/ "jabber-history.txt")))
  :commands (init-jabber))

(use-package init-dired
  :load-path "packages/dired"

  :init (progn
          (add-hook 'dired-mode-hook 'init-dired))

  :commands (init-dired)

  :bind (("C-x C-d" . dired/switch-or-jump)
         :map dired-mode-map
         ("<backspace>" . dired-up-directory)
         ("/" . dired-narrow-fuzzy)))

(use-package rsync
  :load-path "fs"
  :commands (init-rsync))

(use-package ssh
  :load-path "fs"
  :bind (("C-x y s" . ssh-connect))
  :commands (init-ssh))

;; Some unsorted stuff:

(use-package elfeed  ;; customize rmh-elfeed-org-files in init-local
  :config
  (use-package elfeed-org
    :config
    (elfeed-org)
    :ensure t)

  :ensure t)

(use-package init-web
  :load-path "prog")

(use-package init-ido
  :load-path "packages/ido")

(use-package imenu-anywhere
  :config (ido-everywhere)
  :ensure t)

(use-package init-editing-utils
  :load-path "lisp")

(use-package init-paredit
  :load-path "lisp")

(use-package init-lisp
  :load-path "lisp")

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-M-n" . org-forward-heading-same-level)
         ("C-M-p" . org-backward-heading-same-level))
  :config (progn
            (use-package org-fstree
              :ensure t)

            (use-package org-crypt
              :disabled t
              :config
              (org-crypt-use-before-save-magic)
              (setq org-tags-exclude-from-inheritance (quote ("crypt")))
              (setq org-crypt-key nil))

            (defun org-archive-done-tasks ()
              (interactive)
              (org-map-entries 'org-archive-subtree "/DONE" 'file))

            (setq-default org-log-done t
                          org-completion-use-ido t
                          org-edit-timestamp-down-means-later t
                          org-agenda-start-on-weekday nil
                          org-agenda-span 14
                          org-agenda-include-diary t
                          org-agenda-window-setup 'current-window
                          org-fast-tag-selection-single-key 'expert
                          org-export-kill-product-buffer-when-displayed t
                          org-tags-column 80
                          org-refile-use-outline-path (quote file)
                          org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
                          org-outline-path-complete-in-steps t
                          org-ellipsis "..."
                          org-hide-leading-stars t
                          org-startup-indented t
                          org-id-locations-file (tmp/ "org-id-locations.txt")
                          org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "DELEGATED(D@/!)" "TESTING(T)" "PREPARED(p)" "|" "DONE(d!/!)")
                                                    (sequence "WAITING(w!/!)" "SOMEDAY(S)" "|" "CANCELLED(c!/!)"))))

            (use-package org-clock
              :init (progn
                      (setq-default org-clock-persistence-insinuate t
                                    org-clock-persist t
                                    org-clock-in-resume t
                                    org-clock-in-switch-to-state "STARTED"
                                    org-clock-out-remove-zero-time-clocks t)))

            (use-package org-babel
              :init (progn
                      (org-babel-do-load-languages
                       'org-babel-load-languages
                       '((python . t)
                         (sql . t)
                         (sh . t)))
                      (setq org-src-fontify-natively t)))

            (add-hook 'org-mode-hook (lambda () (modify-syntax-entry (string-to-char "") "w")))
            (setq org-startup-align-all-tables "align"))
  :ensure t)

(use-package locales
  :init (progn
          (defun sanityinc/utf8-locale-p (v)
            "Return whether locale string V relates to a UTF-8 locale."
            (and v (string-match "UTF-8" v)))

          (defun locale-is-utf8-p ()
            "Return t iff the \"locale\" command or environment variables prefer UTF-8."
            (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
                (sanityinc/utf8-locale-p (getenv "LC_ALL"))
                (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
                (sanityinc/utf8-locale-p (getenv "LANG"))))

          (when (or window-system (locale-is-utf8-p))
            (setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
            (set-language-environment 'utf-8)
            (setq locale-coding-system 'utf-8)
            (set-default-coding-systems 'utf-8)
            (set-terminal-coding-system 'utf-8)
            (unless (eq system-type 'windows-nt)
              (set-selection-coding-system 'utf-8))
            (prefer-coding-system 'utf-8))

          (use-package ucs-utils)
          (use-package unicode-fonts
            :config (progn
                      (unicode-fonts-setup)))
          (use-package font-utils)))

(use-package multiple-cursors
  :commands
  (mc/mark-next-like-this
   mc/mark-previous-like-this
   mc/mark-all-like-this)
  :config
  (setq-default mc/list-file (tmp/ "multiple-cursors-data.el"))
  :ensure t)

(use-package regex-tool
  :commands regex-tool
  :ensure t)

(use-package pdf-tools
  :commands pdf-tools-install
  :ensure t)

;; (use-package nhexl-mode)

;; https://github.com/kiwanami/emacs-calfw
(use-package calfw
  :config
  (require 'calfw-org))

(use-package magit
  :commands (magit-status
             magit-blame
             magit-commit
             magit-push-current
             magit-log-buffer-file)
  :config (progn
            (setq magit-completing-read-function 'magit-ido-completing-read))
  :bind (("C-x g s" . magit-status)
         ("C-x g b" . magit-blame)
         ("C-x g l" . magit-log-buffer-file)
         ("C-x g c" . magit-commit)
         ("C-x g p c" . magit-push-current))
  :ensure t)

(use-package monky
  :load-path "packages/vcs/monky")

(use-package bookmark+
  :init (progn
          (setq-default bmkp-bmenu-stat-file (tmp/ "emacs-bmk-bmenu-state.el")
                        bookmark-default-file (dropbox/ "bookmarks.txt")
                        bookmark-save-flag t))
  :ensure t)


(use-package google-translate
  :commands translate-text
  :config
  (defun translate-text (sentence)
    "Google translate without specifying language."
    (interactive "sTranslate sentence: ")
    (setq-default lang-regexes '(("[a-zA-Z]" . ("en" "ru"))
                                 ("[а-яА-Я]" . ("ru" "en"))))
    (dolist (lang-regex lang-regexes)
      (if (string-match (car lang-regex) sentence)
          (google-translate-translate (nth 1 lang-regex) (nth 2 lang-regex) sentence))))
  :ensure t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package my/process-management
  :init (progn
          (use-package elscreen
            :config
            (elscreen-start)
            (setq elscreen-display-tab nil)
            :ensure t)

          ;; https://github.com/ilya-babanov/emacs-bpr
          (use-package bpr
            :config
            (setq bpr-colorize-output t
                  bpr-close-after-success t
                  bpr-erase-process-buffer t
                  bpr-show-progress nil
                  bpr-open-after-error nil)
            :ensure t)

          (use-package dizzee
            :commands (dz-defservice dz-defservice-group)
            :config
            (defun dz-restart-current ()
              (interactive)
              (setq dz-buffer-name (replace-regexp-in-string "*" "" (buffer-name)))
              (setq dz-restart-expr (concatenate 'string dz-buffer-name "-restart"))
              (funcall (intern dz-restart-expr)))

            (defun dz-stop-current ()
              (interactive)
              (setq dz-buffer-name (replace-regexp-in-string "*" "" (buffer-name)))
              (setq dz-restart-expr (concatenate 'string dz-buffer-name "-stop"))
              (funcall (intern dz-restart-expr)))

            (global-set-key (kbd "<f5>") 'dz-restart-current)
            (global-set-key (kbd "<f4>") 'dz-stop-current)
            :ensure t)

          (use-package prodigy
            :commands (prodigy
                       prodigy-start-all-services)
            :config (progn
                      (defun ido-prodigy-menu ()
                        (interactive)
                        (let* ((ido-prodigy-choices (mapcar (lambda (serv)
                                                              (let* ((status (prodigy-service-started-p serv))
                                                                     (service-name (cadr serv))
                                                                     (stopped-label service-name)
                                                                     (started-label  service-name))
                                                                (if status started-label
                                                                  stopped-label)))
                                                            prodigy-services)))
                          (message (ido-completing-read "Service: " ido-prodigy-choices))))

                      (defun find-prodigy-service-with-name (service-name)
                        (let ((matches (-filter (lambda (s) (string= service-name (cadr s)))
                                                prodigy-services)))
                          (car matches)))

                      (defun prodigy-apply-to-services (services fn)
                        (prodigy-with-refresh
                         (-each services fn)))

                      (defun prodigy-start-all-services ()
                        (interactive)
                        (prodigy-apply-to-services prodigy-services
                                                   'prodigy-start-service))

                      (defun prodigy-stop-all-services ()
                        (interactive)
                        (prodigy-apply-to-services prodigy-services
                                                   'prodigy-stop-service))

                      ;; TODO pull this feature
                      (defun prodigy-stop-services-with-tag (tag)
                        (interactive "MTag: ")
                        (prodigy-apply-to-services
                         (prodigy-services-tagged-with (intern tag))
                         'prodigy-stop-service))

                      ;; TODO pull this feature
                      (defun prodigy-start-services-with-tag (tag)
                        (interactive "MTag: ")
                        (prodigy-apply-to-services
                         (prodigy-services-tagged-with (intern tag))
                         'prodigy-start-service))
                      (prodigy-define-default-status-list)
                      (prodigy-define-tag
                        :name 'django
                        :ready-message "Quit the server with CONTROL-C"))
            :ensure t)))

(use-package super-save
  :config
  (super-save-initialize)
  (setq super-save-auto-save-when-idle t)
  :ensure t)

(use-package general
  :config
  (general-define-key
   :keymaps 'global
   "C-<" 'mc/mark-previous-like-this
   "C->" 'mc/mark-next-like-this
   "C-+" 'mc/mark-all-like-this)

  (general-define-key
   :prefix "C-x i"
   "m" 'imenu-anywhere)

  (general-define-key
   :prefix "C-x y"
   "t t" 'translate-text
   "p" 'prodigy
   "f f" 'toggle-frame-fullscreen
   "i" 'yas-ido-expand)

  (general-define-key
   :keymaps 'ido-completion-map
   "C-n" 'ido-next-match
   "C-p" 'ido-prev-match)
  :ensure t)

(require 'init-local nil t)

(use-package beautifies
  :load-path "packages/core")

(provide 'init)
;;; init.el ends here
