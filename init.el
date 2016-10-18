;;; init.el --- my emacs configuration
;;
;; Filename: init.el
;; Description: my emacs configuration
;; Author: Dmitry Akatov
;; Created: Sun Aug 09 21:49:00 2015 (-0400)
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d
;; Keywords: Emacs 24.5
;; Compatibility: emacs >= 24.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(package-refresh-contents t)
(if (not (package-installed-p 'use-package))
    (package-install 'use-package))
(require 'use-package)
(use-package use-package
  :no-require t
  :ensure diminish
  :ensure bind-key)

(setq custom-file (concat user-emacs-directory "custom.el"))
(dolist (key '("\C-l" "\C-t" "\C-xi" "\C-cC-b"))
  (global-unset-key key))
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(use-package ui
  :load-path "core")

(use-package mac
  :load-path "core")

(use-package sh
  :load-path "core")

(use-package buffers
  :load-path "core"
  :bind (("C-x o" . switch-window)
         ("C-x 1" . delete-other-windows)
         ("C-x 2" . split-window-vertically)
         ("C-x 3" . split-window-horizontally))
  :commands (delete-this-file
             rename-this-file-and-buffer))

(use-package snippets
  :load-path "editor"
  :commands (yas-ido-expand
             yas-new-snippet))

(use-package checkers
  :load-path "editor")

(use-package macro
  :load-path "editor"
  :commands save-macro)

(use-package ac
  :load-path "editor"
  :bind ("M-/" . hippie-expand))

(use-package folding
  :load-path "editor"
  :bind (:map prog-mode-map
              ("C-c f t" . origami-toggle-node)
              ("C-c f r" . origami-recursively-toggle-node)
              ("C-c f o" . origami-show-only-node)))

(use-package search
  :load-path "editor")

(use-package log
  :load-path "editor"
  :commands (itail
             mwe:log-keyboard-commands))

(use-package prog-mode
  :init (progn
          (use-package web-mode
            :mode ("\\.html\\'" "\\.ejs\\'" "\\.htm\\'" "\\.jsx\\'")
            :config (progn
                      (use-package web-beautify
                        :ensure t)

                      (use-package emmet-mode
                        :mode ("\\.html\\'" "\\.htm\\'")
                        :config (add-hook 'web-mode-hook 'emmet-mode)
                        :ensure t)

                      (setq web-mode-markup-indent-offset 4
                            web-mode-css-indent-offset 4
                            web-mode-code-indent-offset 4))
            :ensure t)

          (use-package restclient
            :commands restclient-mode
            :mode ("\\.rest\\'" . restclient-mode)
            :ensure t)

          (use-package javascript
            :init (progn
                    (use-package json-mode
                      :commands (json-mode json-reformat-region)
                      :ensure t)
                    (use-package js2-mode
                      :mode ("\\.js\\'" . js2-mode)
                      :commands js2-mode
                      :ensure t)
                    (use-package ac-js2
                      :commands ac-js2
                      :ensure t)
                    (use-package js-comint
                      :commands js-comint
                      :ensure t)))

          (use-package scala-mode
            :ensure t)

          (use-package python
            :mode ("\\.py\\'" . python-mode)
            :interpreter ("ipython" . python-mode)
            :load-path "python/"
            :init (progn
                    ;; (use-package jedi
                    ;;   :config (progn
                    ;;             (add-hook 'python-mode-hook 'jedi:setup)
                    ;;             (setq jedi:complete-on-dot t)
                    ;;             (jedi:install-server))
                    ;;   :ensure t)

                    (use-package pungi
                      :ensure t)

                    (use-package cinspect
                      :ensure t)

                    (use-package py-isort
                      :ensure t)

                    (use-package py-yapf
                      :ensure t)

                    (use-package virtualenv
                      :config (progn
                                (use-package virtualenvwrapper
                                  :config (progn
                                            (venv-initialize-interactive-shells)
                                            (venv-initialize-eshell))
                                  :ensure t))
                      :ensure t))

            :config (progn
                      (setq-default python-indent-offset 4)

                      (defun python-highlight-breakpoints ()
                        (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

                      (defun python-add-breakpoint ()
                        "Add a break point"
                        (interactive)
                        (insert "import ipdb; ipdb.set_trace()")
                        (python-highlight-breakpoints))

                      (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
                      (add-hook 'python-mode-hook 'linum-mode))
            :ensure t)))

(use-package ido
  :init (progn
          (use-package idomenu
            :ensure t)

          (use-package crm-custom
            :config (crm-custom-mode 1)
            :ensure t)

          (use-package ido-vertical-mode
            :config (progn
                      (setq ido-vertical-define-keys 'C-n-and-C-p-only)
                      (ido-vertical-mode))
            :ensure t)

          (use-package ido-completing-read+
            :ensure t)

          (use-package ido-ubiquitous
            :config (ido-ubiquitous-mode)
            :ensure t)

          (use-package smex
            ;; Smex is a M-x enhancement for Emacs, it provides a convenient interface to
            ;; your recently and most frequently used commands.
            :ensure t)

          (use-package imenu-anywhere
            :config (ido-everywhere)
            :ensure t)))
:config (progn
          (setq ido-enable-flex-matching t
                ido-use-filename-at-point nil
                ido-auto-merge-work-directories-length -1
                ido-use-virtual-buffers t
                ido-confirm-unique-completion t
                ido-default-buffer-method 'selected-window)

          (global-set-key [remap execute-extended-command] 'smex)
          (defadvice ido-find-file (after find-file-sudo activate)
            "Find file as root if necessary."
            (unless (and buffer-file-name
                         (file-writable-p buffer-file-name))
              (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
          (defun bind-ido-keys ()
            "Keybindings for ido mode."
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
          (add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))
          (add-hook 'ido-setup-hook #'bind-ido-keys)
          (ido-mode))

(use-package init-editing-utils
  :load-path "lisp")

(use-package init-paredit
  :load-path "lisp")

(use-package init-lisp
  :load-path "lisp")

(use-package org
  :config (progn
            (use-package org-fstree
              :ensure t)

            (use-package org-crypt
              :disabled t
              :config
              (org-crypt-use-before-save-magic)
              (setq org-tags-exclude-from-inheritance (quote ("crypt")))
              (setq org-crypt-key nil))

            (define-key global-map (kbd "C-c l") 'org-store-link)
            (define-key global-map (kbd "C-c a") 'org-agenda)
            (define-key global-map (kbd "C-c c") 'org-capture)
            (define-key org-mode-map (kbd "C-M-n") 'org-forward-heading-same-level)
            (define-key org-mode-map (kbd "C-M-p") 'org-backward-heading-same-level)

            (setq org-log-done t
                  org-completion-use-ido t
                  org-edit-timestamp-down-means-later t
                  org-agenda-start-on-weekday nil
                  org-agenda-span 14
                  org-agenda-include-diary t
                  org-agenda-window-setup 'current-window
                  org-fast-tag-selection-single-key 'expert
                  org-export-kill-product-buffer-when-displayed t
                  org-tags-column 80)

            ;; org as word processor
            ;; (use-package my/org-word-processor
            ;;   :init (progn
            ;;           (setq org-hide-emphasis-markers t)
            ;;           (font-lock-add-keywords 'org-mode
            ;;                                   '(("^ +\\([-*]\\) "
            ;;                                      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
            ;;           (use-package org-bullets
            ;;             :ensure t)
            ;;           (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

            ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
            (setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
            ;; Targets start with the file name - allows creating level 1 tasks
            (setq org-refile-use-outline-path (quote file))
            ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
            (setq org-outline-path-complete-in-steps t)
            (setq org-todo-keywords
                  (quote ((sequence "TODO(t)" "STARTED(s)" "DELEGATED(D@/!)" "TESTING(T)" "PREPARED(p)" "|" "DONE(d!/!)")
                          (sequence "WAITING(w!/!)" "SOMEDAY(S)" "|" "CANCELLED(c!/!)"))))

            (setq org-ellipsis "..." )
            (setq org-hide-leading-stars t)
            (setq org-startup-indented t)

            (use-package org-clock
              :init (progn
                      (setq org-clock-persistence-insinuate t
                            org-clock-persist t
                            org-clock-in-resume t
                            org-clock-in-switch-to-state "STARTED"
                            org-clock-out-remove-zero-time-clocks t)))

            (use-package org-babel
              :init (progn
                      (org-babel-do-load-languages
                       'org-babel-load-languages
                       '((python . t)
                         (sql . t)))
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
  :commands (mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this)
  :ensure t)

(use-package regex-tool
  :commands regex-tool
  :ensure t)

(use-package pdf-tools
  :commands pdf-tools-install
  :ensure t)

(use-package hl-line+
  :config (progn
            (set-face-background hl-line-face "#363636")))

(use-package camcorder
  :commands camcorder-mode)

(use-package nhexl-mode)

;; https://github.com/kiwanami/emacs-calfw
(use-package calfw
  :config
  (require 'calfw-org))

(use-package keyfreq
  :config (progn
            (keyfreq-mode t)
            (keyfreq-autosave-mode t)))

(use-package magit
  :commands (magit-status
             magit-blame
             magit-commit
             magit-push-current
             magit-log-buffer-file)
  :config (progn
            (use-package git-gutter+
              :config (progn
                        (global-git-gutter+-mode))
              :ensure t)
            (setq magit-completing-read-function 'magit-ido-completing-read))
  :bind (("C-x g s" . magit-status)
         ("C-x g b" . magit-blame)
         ("C-x g l" . magit-log-buffer-file)
         ("C-x g c" . magit-commit)
         ("C-x g p c" . magit-push-current))
  :ensure t)

(use-package my/project-management
  :init (progn
          (use-package bookmark+
            :ensure t)))

(use-package my/internet-services
  :init (progn
          (use-package hackernews
            :commands hackernews
            :ensure t)

          (use-package google-translate
            :commands translate-text
            :config (defun translate-text (sentence)
                      "Google translate without specifying language"
                      (interactive "sTranslate sentence: ")
                      (setq lang-regexes '(("[a-zA-Z]" . ("en" "ru"))
                                           ("[а-яА-Я]" . ("ru" "en"))))
                      (dolist (lang-regex lang-regexes)
                        (if (string-match (car lang-regex) sentence)
                            (google-translate-translate (nth 1 lang-regex) (nth 2 lang-regex) sentence))))
            :ensure t)))

(use-package frame-cmds)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package my/process-management
  :init (progn
          (use-package elscreen
            :config (progn
                      (elscreen-start)
                      (setq elscreen-display-tab nil))
            :ensure t)

          (use-package lice
            :commands lice
            :ensure t)

          ;; https://github.com/ilya-babanov/emacs-bpr
          (use-package bpr
            :config (progn
                      (setq bpr-colorize-output t
                            bpr-close-after-success t
                            bpr-erase-process-buffer t
                            bpr-show-progress nil
                            bpr-open-after-error nil))
            :ensure t)

          (use-package dizzee
            :commands (dz-defservice dz-defservice-group)
            :config (progn
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
                      (global-set-key (kbd "<f4>") 'dz-stop-current)))

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

(use-package my/dired
  :init (progn
          (use-package dired)

          (use-package dired-subtree
            :ensure t)

          (use-package dired-filetype-face
            :ensure t)

          (defun mydired-sort ()
            "Sort dired listings with directories first."
            (save-excursion
              (let (buffer-read-only)
                (forward-line 2) ;; beyond dir. header
                (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
              (set-buffer-modified-p nil)))

          (add-hook 'dired-after-readin-hook 'hl-line-mode)
          (add-hook 'dired-after-readin-hook 'mydired-sort)
          (add-hook 'dired-after-readin-hook 'dired-omit-mode)

          (dired-omit-mode 1)
          (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

          (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)
          ;; (define-key dired-mode-map (kbd "<return>") 'dired-subtree-toggle)
          (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)))

(use-package my/http
  :init (progn
          (use-package http  ;; https://github.com/emacs-pe/http.el
            :ensure t)))

(use-package super-save
  :config (progn
            (super-save-initialize)
            (setq super-save-auto-save-when-idle t))
  :ensure t)

(use-package auto-rsync
  :load-path "packages/custom/"
  :config (progn
            (auto-rsync-mode t)))

(use-package general
  :config (progn
            (general-define-key
             :keymaps 'global
             "C-<" 'mc/mark-previous-like-this
             "C->" 'mc/mark-next-like-this
             "C-+" 'mc/mark-all-like-this)

            (general-define-key
             :prefix "C-x i"
             "m" 'imenu-anywhere
             "t" 'iterm-goto-filedir-or-home)

            (general-define-key
             :prefix "C-x y"
             "t t" 'translate-text
             "p" 'prodigy
             "f f" 'toggle-frame-fullscreen
             "i" 'yas-ido-expand)

            (general-define-key
             :keymaps 'python-mode-map
             :prefix "C-c"
             "C-b" 'python-add-breakpoint)

            (general-define-key
             :keymaps 'ido-completion-map
             "C-n" 'ido-next-match
             "C-p" 'ido-prev-match))
  :ensure t)

(require 'init-local nil t)

(provide 'init)

;;; init.el ends here
