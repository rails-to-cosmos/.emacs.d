;;; init.el --- my emacs configuration
;;; Commentary:
;;; Code:

(require 'package)

(dolist (package-archive
         '(("melpa" . "http://melpa.milkbox.net/packages/")
           ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
           ("marmalade" . "http://marmalade-repo.org/packages/")
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
  :load-path "packages/core"
  :commands (after-load ;; After FEATURE is loaded, evaluate BODY.
              ))

(use-package directories
  :load-path "packages/core"
  :commands (tmp/ ;; contains temporary files.
             shared/ ;; contains shared files.
             ))

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
  :bind (("C-x o" . ace-window)
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

(use-package expand-region
  :config (progn
            (setq shift-select-mode nil))
  :bind ("C-=" . er/expand-region)
  :ensure t)

;; (use-package embrace
;;   :config (progn
;;             (add-hook 'org-mode-hook #'embrace-org-mode-hook))
;;   :bind (("C-c e" . embrace-commander))
;;   :ensure t)

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
            (add-to-list 'yas-snippet-dirs (shared/ "snippets")))
  :commands (yas-ido-expand
             yas-new-snippet
             yas-recompile-all
             yas-reload-all))

(use-package thinks
  :config (progn
            (setq thinks-from 'bottom-diagonal))
  :ensure t)

(use-package macro
  :load-path "packages/editor"
  :bind (("C-c +" . increment-number-at-point)
         ("C-c -" . decrement-number-at-point))
  :commands (save-macro))

(use-package company
  :diminish company-mode
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

(use-package init-c
  :load-path "packages"
  :init (progn
          (add-hook 'c++-mode-hook 'irony-mode)
          (add-hook 'c-mode-hook 'irony-mode)
          (add-hook 'objc-mode-hook 'irony-mode)
          (add-hook 'irony-mode-hook 'irony-replace-completion-at-point)
          (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
  :commands (irony-mode
             irony-replace-completion-at-point))

(use-package init-python
  :load-path "prog"

  :diminish (rainbow-delimiters-mode
             rainbow-mode)

  :init (progn
          ;; (autoload 'elpy-mode-map "elpy" "Define elpy-mode-map." t)
          ;;(add-hook 'python-mode-hook 'elpy-mode)
          (add-hook 'python-mode-hook 'init-python)
          (add-hook 'python-mode-hook 'python-highlight-breakpoints)
          (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
          (add-hook 'python-mode-hook 'linum-mode))

  :mode ("\\.py\\'" . python-mode)

  :bind (:map python-mode-map
              ("C-c C-b" . python-add-breakpoint)
              ;; ("C-c C-g" . jedi:goto-definition)
              )

  :commands (init-python
             python-add-breakpoint
             python-highlight-breakpoints
             ;; jedi:install-server
             ;; jedi:goto-definition
             ;; elpy-mode
             ))

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
             jabber-global-history-filename (shared/ "jabber-history.txt")))
  :commands (init-jabber))

(use-package init-dired
  :load-path "packages/dired"

  :init (progn
          (add-hook 'dired-before-readin-hook 'dired/hide-cursor)
          (add-hook 'dired-before-readin-hook 'hl-line-mode)
          (add-hook 'dired-before-readin-hook 'dired-omit-mode)
          (add-hook 'dired-mode-hook 'install-dired-x))

  :commands (install-dired-x
             dired/hide-cursor
             dired/sort)

  :bind (("C-x C-d" . dired/switch-or-jump)
         :map dired-mode-map
         ("<backspace>" . dired-up-directory)
         ("/" . dired-narrow-fuzzy)))

(use-package rsync
  :load-path "packages"
  :commands (rsync-enable
             rsync-disable))

(use-package ssh
  :load-path "packages"
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
  :diminish (org-indent-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ;; ("C-c C-l" . my-org-insert-link)
         ("C-M-n" . ded/org-show-next-heading-tidily)
         ("C-M-p" . ded/org-show-previous-heading-tidily))
  :config (progn
            (setq org-confirm-elisp-link-not-regexp "org-open-file")

            ;; Mark heading done when all checkboxes are checked
            ;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
            (eval-after-load 'org-list
              '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

            (defun ndk/checkbox-list-complete ()
              (save-excursion
                (org-back-to-heading t)
                (let ((beg (point)) end)
                  (end-of-line)
                  (setq end (point))
                  (goto-char beg)
                  (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
                      (if (match-end 1)
                          (if (equal (match-string 1) "100%")
                              ;; all done - do the state change
                              (org-todo 'done)
                            (org-todo 'todo))
                        (if (and (> (match-end 2) (match-beginning 2))
                                 (equal (match-string 2) (match-string 3)))
                            (org-todo 'done)
                          (org-todo 'todo)))))))

            ;; Org-link: insert heading by default
            (require 'mm-url) ; to include mm-url-decode-entities-string

            (defun my-org-insert-link ()
              "Insert org link where default description is set to html title."
              (interactive)
              (let* ((url (read-string "URL: "))
                     (title (get-html-title-from-url url)))
                (org-insert-link nil url title)))

            (defun get-html-title-from-url (url)
              "Return content in <title> tag."
              (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
                (save-excursion
                  (set-buffer download-buffer)
                  (beginning-of-buffer)
                  (setq x1 (search-forward "<title>"))
                  (search-forward "</title>")
                  (setq x2 (search-backward "<"))
                  (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)))))

            (defun ded/org-show-next-heading-tidily ()
              "Show next entry, keeping other entries closed."
              (interactive)
              (if (save-excursion (end-of-line) (outline-invisible-p))
                  (progn (org-show-entry) (show-children))
                (org-forward-heading-same-level 1 t)
                (unless (and (bolp) (org-on-heading-p))
                  (org-up-heading-safe)
                  (hide-subtree)
                  (error "Boundary reached"))
                (org-overview)
                (org-reveal t)
                (org-show-entry)
                (show-children)))

            (defun ded/org-show-previous-heading-tidily ()
              "Show previous entry, keeping other entries closed."
              (interactive)
              (let ((pos (point)))
                (org-backward-heading-same-level 1 t)
                (unless (and (< (point) pos) (bolp) (org-on-heading-p))
                  (goto-char pos)
                  (hide-subtree)
                  (error "Boundary reached"))
                (org-overview)
                (org-reveal t)
                (org-show-entry)
                (show-children)))

            (defface hi-yellow-b
              '((t (:foreground "yellow"))) "Highlight" :group 'hi)

            (defun my/org-highlight ()
              "Highlight something."
              (interactive)
              (hi-lock-mode t)
              (highlight-regexp "\wOK\w" 'hi-green-b)
              (highlight-regexp "SUCCESS" 'hi-green-b)
              (highlight-regexp "FINISHED" 'hi-green-b)
              (highlight-regexp "IN PROGRESS" 'hi-yellow-b)
              (highlight-regexp "LOADING\, PLEASE WAIT\.\.\." 'hi-yellow-b)
              (highlight-regexp "FAILED" 'hi-red-b)
              (highlight-regexp "FAILURE" 'hi-red-b)
              (highlight-regexp "FAIL" 'hi-red-b))
            (add-hook 'org-mode-hook 'my/org-highlight)

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
              (org-map-entries 'org-archive-subtree "/DONE" 'file)
              (org-map-entries 'org-archive-subtree "/CANCELLED" 'file))

            (setq-default
             ;; org-log-done t
             org-special-ctrl-a/e t
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
             org-ellipsis " ↓"
             org-hide-leading-stars t
             org-startup-indented t
             org-id-locations-file (tmp/ "org-id-locations.txt")
             org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "DELEGATED(D@/!)" "TESTING(T)" "PREPARED(p)" "|" "DONE(d)")
                                       (sequence "WAITING(w)" "SOMEDAY(S)" "|" "CANCELLED(c)"))))

            (use-package org-clock
              :init (progn
                      (setq-default
                       org-clock-persistence-insinuate t
                       org-clock-persist t
                       org-clock-in-resume t
                       org-clock-in-switch-to-state "STARTED"
                       org-clock-out-remove-zero-time-clocks t)))

            (use-package org-dashboard
              :ensure t)

            (use-package org-babel
              :init (progn
                      (use-package ob-ipython
                        :config (progn
                                  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
                                  (setq python-shell-prompt-detect-failure-warning nil)

                                  (defun ob-ipython-inline-image (b64-string)
                                    "Write the b64-string to a temporary file. Returns an org-link to the file."
                                    (let* ((tfile (make-temp-file "ob-ipython-" nil ".png"))
                                           (link (format "[[file:%s]]" tfile)))
                                      (ob-ipython--write-base64-string tfile b64-string)
                                      link))

                                  (defun org-babel-execute:ipython (body params)
                                    "Execute a block of IPython code with Babel. This function is called by `org-babel-execute-src-block'."
                                    (let* ((file (cdr (assoc :file params)))
                                           (session (cdr (assoc :session params)))
                                           (result-type (cdr (assoc :result-type params))))
                                      (org-babel-ipython-initiate-session session params)
                                      (-when-let (ret (ob-ipython--eval
                                                       (ob-ipython--execute-request
                                                        (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                                                       params (org-babel-variable-assignments:python params))
                                                        (ob-ipython--normalize-session session))))
                                        (let ((result (cdr (assoc :result ret)))
                                              (output (cdr (assoc :output ret))))
                                          (if (eq result-type 'output)
                                              (concat
                                               output
                                               (format "%s"
                                                       (mapconcat 'identity
                                                                  (loop for res in result
                                                                        if (eq 'image/png (car res))
                                                                        collect (ob-ipython-inline-image (cdr res)))
                                                                  "\n")))
                                            (ob-ipython--create-stdout-buffer output)
                                            (cond ((and file (string= (f-ext file) "png"))
                                                   (->> result (assoc 'image/png) cdr (ob-ipython--write-base64-string file)))
                                                  ((and file (string= (f-ext file) "svg"))
                                                   (->> result (assoc 'image/svg+xml) cdr (ob-ipython--write-string-to-file file)))
                                                  (file (error "%s is currently an unsupported file extension." (f-ext file)))
                                                  (t (->> result (assoc 'text/plain) cdr)))))))))
                        :ensure t)

                      (use-package ob-async
                        :load-path "packages/ob-async"
                        :config (progn
                                  (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)))

                      (add-hook 'org-mode-hook 'org-hide-block-all)

                      (org-babel-do-load-languages
                       'org-babel-load-languages
                       '((python . t)
                         (ipython . t)
                         (sql . t)
                         (C . t)
                         (shell . t)))
                      (setq org-src-fontify-natively t)
                      (setq org-confirm-babel-evaluate nil)
                      (setq org-confirm-shell-link-function nil)
                      (setq org-confirm-elisp-link-function nil)))

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
;; (use-package calfw
;;   :config
;;   (require 'calfw-org))

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

(use-package bookmark+
  :init (progn
          (setq-default bmkp-bmenu-stat-file (tmp/ "emacs-bmk-bmenu-state.el")
                        bookmark-default-file (shared/ "bookmarks.txt")
                        bookmark-save-flag t))
  :ensure t)

(use-package google-translate
  :commands translate-text
  :config (progn
            (defun translate-text (sentence)
              "Google translate without specifying language."
              (interactive "sTranslate sentence: ")
              (setq-default lang-regexes '(("[a-zA-Z]" . ("en" "ru"))
                                           ("[а-яА-Я]" . ("ru" "en"))))
              (dolist (lang-regex lang-regexes)
                (if (string-match (car lang-regex) sentence)
                    (google-translate-translate (nth 1 lang-regex) (nth 2 lang-regex) sentence)))))
  :bind ("C-x y t t" . translate-text)
  :ensure t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package perspeen
  :init (progn
          (setq perspeen-use-tab nil))
  :config (progn
            (perspeen-mode))
  :ensure t)

(use-package powerline
  :config (progn
            (powerline-default-theme)
            (setq powerline-default-separator nil))
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
              :ready-message "Quit the server with CONTROL-C")
            (prodigy-define-service
              :name "Jupyter Notebook"
              :command "jupyter"
              :args '("notebook")
              :cwd "~/Dropbox/Documents/SHAD"
              :tags '(shad)
              :stop-signal 'sigkill
              :kill-process-buffer-on-stop t))
  :ensure t)

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
