;;; init.el --- emacs configuration by Dmitry Akatov
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


;; Init use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(eval-when-compile
  (require 'use-package))
(use-package diminish
  :commands diminish
  :ensure t)
(use-package bind-key
  :ensure t)

;; TODO specify paths
(defvar emacs-persistence-directory (concat user-emacs-directory "persistence/"))
(make-directory emacs-persistence-directory t)
(defvar savehist-file (concat emacs-persistence-directory ".minibuffer-history"))

(setq session-save-file (concat emacs-persistence-directory ".session")
      frame-restore-parameters-file (concat emacs-persistence-directory ".frame-restore-parameters")
      bmkp-bmenu-state-file (concat emacs-persistence-directory ".emacs-bmk-bmenu-state")
      bookmark-default-file (concat emacs-persistence-directory ".bookmarks")
      desktop-path (list emacs-persistence-directory)
      recentf-save-file (concat emacs-persistence-directory ".recentf")
      custom-file (concat emacs-persistence-directory ".custom")
      mc/list-file (concat emacs-persistence-directory ".mc-lists")
      abbjrev-file-name (concat emacs-persistence-directory ".abbrev-defs"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(use-package autocomplete
  :config (progn
          (use-package popup)
          (use-package pos-tip)
          (use-package popup-kill-ring)
          (use-package auto-complete-config)
          (ac-config-default)
          (defvar ac-comphist-file (concat emacs-persistence-directory ".ac-comphist"))
          (defvar ac-use-menu-map t)
          (defvar hippie-expand-verbose t)
          (defvar smart-tab-using-hippie-expand t
            "turn this on if you want to use hippie-expand completion.")
          (setq hippie-expand-try-functions-list
                '(yas/hippie-try-expand
                  try-complete-file-name-partially
                  try-expand-all-abbrevs
                  try-expand-dabbrev
                  try-expand-dabbrev-all-buffers
                  try-expand-dabbrev-from-kill
                  try-complete-lisp-symbol-partially
                  try-complete-lisp-symbol))))

(use-package eshell
  :init (progn
          (use-package exec-path-from-shell
            :ensure t
            :config (progn
                      (when (memq window-system '(mac ns))
                        (exec-path-from-shell-initialize)
                        (setenv "LANG" "en_US.UTF-8")
                        (setenv "LC_ALL" "en_US.UTF-8")
                        (setenv "LC_CTYPE" "en_US.UTF-8")))))
  :config (progn
            (defun spawn-shell (name &rest commands)
              "Invoke shell with commands"
              (interactive "MName of shell buffer to spawn: ")
              (pop-to-buffer (get-buffer-create name))
              (setq default-eshell-buffer-name
                    (if (string= (boundp 'eshell-buffer-name) nil)
                        "*eshell*"
                      eshell-buffer-name))
              (setq eshell-buffer-name name)
              (eshell)
              (setq eshell-buffer-name default-eshell-buffer-name)
              (loop for command in commands
                    do (insert (concat command "\n")))
              (eshell-send-input)
              (goto-char (point-max)))

            (defun eshell-init-aliases()
              (add-to-list 'eshell-command-aliases-list '("ff" "find-file"))
              (add-to-list 'eshell-command-aliases-list '("d" "dired $1"))
              (add-to-list 'eshell-command-aliases-list '("l" "ls"))
              (add-to-list 'eshell-command-aliases-list '("ll" "ls -la"))
              (add-to-list 'eshell-command-aliases-list '("pip-update" "pip freeze --local | grep -v '^\\-e' | cut -d = -f 1  | xargs -n1 pip install -U")))
            (add-hook 'eshell-mode-hook 'eshell-init-aliases)))

;;----------------------------------------------------------------------------
;; System constants
;;----------------------------------------------------------------------------

(defconst *is-a-mac* (eq system-type 'darwin))
(setq mac-command-modifier 'meta)

;;----------------------------------------------------------------------------
;; User interface
;;----------------------------------------------------------------------------

(use-package init-gui-frames
  :if window-system
  :config (progn
            (toggle-max-frame)))

(use-package smart-mode-line
  :config (progn
            (setq-default
             mode-line-format
             '("%e"
               mode-line-front-space
               mode-line-mule-info
               mode-line-client
               mode-line-modified
               mode-line-remote
               mode-line-frame-identification
               mode-line-buffer-identification
               "   "
               mode-line-position
               (vc-mode vc-mode)
               "  "
               mode-line-modes
               mode-line-misc-info
               mode-line-end-spaces)))
  :ensure t)

(use-package miniedit
  :init (progn
          (miniedit-install))
  :ensure t)

;;----------------------------------------------------------------------------
;; elisp utils
;;----------------------------------------------------------------------------

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(use-package google-translate
  :config (defun translate-text (sentence)
            "Google translate without specifying language"
            (interactive "sTranslate sentence: ")
            (setq lang-regexes '(("[a-zA-Z]" . ("en" "ru"))
                                 ("[а-яА-Я]" . ("ru" "en"))))
            (dolist (lang-regex lang-regexes)
              (if (string-match (car lang-regex) sentence)
                  (google-translate-translate (nth 1 lang-regex) (nth 2 lang-regex) sentence))))
  :bind ("C-x y t t" . translate-text)
  :ensure t)

(defun save-macro (name)
  "save a macro. Take a name as argument
        and save the last defined macro under
        this name at the end of your .emacs"
  (interactive "SName of the macro:") ; ask for the name of the macro
  (kmacro-name-last-macro name)        ; use this name for the macro
  (find-file user-init-file)   ; open ~/.emacs or other user init file
  (goto-char (point-max))      ; go to the end of the .emacs
  (newline)                    ; insert a newline
  (insert-kbd-macro name)      ; copy the macro
  (newline)                    ; insert a newline
  (switch-to-buffer nil))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))


;;----------------------------------------------------------------------------
;; Text editing utils
;;----------------------------------------------------------------------------

(use-package wgrep
  :ensure t)

(use-package loccur
  :commands (loccur loccur-current loccur-previous-match)
  :bind (("C-o" . loccur-current)
         ("C-M-o" . loccur)
         ("C-S-o" . loccur-previous-match))
  :ensure t)

(use-package mmm-mode
  :config (progn
            (setq mmm-global-mode 'maybe)
            (mmm-add-classes
             '((python-rst
                :submode rst-mode
                :front "^ *[ru]?\"\"\"[^\"]*$"
                :back "^ *\"\"\""
                :include-front t
                :include-back t
                :end-not-begin t)))
            (mmm-add-mode-ext-class 'python-mode nil 'python-rst))
  :ensure t)

;;----------------------------------------------------------------------------
;; Logging
;;----------------------------------------------------------------------------

(use-package mwe-log-commands
  :ensure t)


;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(use-package bookmark+
  :ensure t)
(use-package scratch
  :ensure t)
;; (use-package yasnippet
;;   :config (progn
;;             (yas-global-mode 1)
;;             (bind-key "C-j" 'yas-expand yas-minor-mode-map))
;;   :ensure t)
(use-package emmet-mode
  :commands emmet-mode
  :mode ("\\.html\\'" . emmet-mode)
  :mode ("\\.htm\\'" . emmet-mode)
  :ensure t)
(use-package impatient-mode
  :commands impatient-mode
  :ensure t)
(use-package restclient
  :commands restclient-mode
  :mode ("\\.rest\\'" . restclient-mode)
  :ensure t)
;; (use-package emacsql
;;   :ensure pg)

(use-package python
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("ipython" . python-mode)
  :load-path "python/"
  :init (progn
          (use-package jedi
            :ensure t)
          (use-package cinspect
            :ensure t)
          (use-package py-isort
            :ensure t)
          (use-package py-yapf
            :ensure t)
          (use-package pyenv-mode
            :ensure t)
          (use-package pyvenv
            :ensure t)
          (use-package yasnippet
            :ensure t)
          (use-package live-py-mode
            :ensure t))
  :config (progn
            (defun python-highlight-breakpoints ()
              (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
            (defun python-add-breakpoint ()
              "Add a break point"
              (interactive)
              (insert "import ipdb; ipdb.set_trace()")
              (python-highlight-breakpoints))
            (add-hook 'python-mode-hook 'linum-mode)
            (add-hook 'python-mode-hook 'jedi:setup)
            (add-hook 'python-mode-hook 'yas-minor-mode)
            (add-hook 'python-mode-hook 'python-highlight-breakpoints)
            (setq python-indent-offset 4)
            (make-directory "~/.virtualenvs" t)
            (jedi:install-server))
  :bind (("C-c C-b" . python-add-breakpoint))
  :ensure t)

(require 'init-isearch)

(use-package uniquify
  :disabled t
  :init (progn
          (setq uniquify-buffer-name-style 'reverse
                uniquify-separator " • "
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*")))

(use-package ibuffer
  :init (progn
          (use-package fullframe
            :ensure t)
          (use-package ibuffer-vc
            :ensure t)
          (use-package ibuffer-git
            :ensure t))
  :config (progn
            (setq ibuffer-formats
                  '((mark modified read-only vc-status-mini " "
                          (name 18 18 :left :elide)
                          " "
                          (size-h 9 -1 :right)
                          " "
                          (mode 16 16 :left :elide)
                          " "
                          filename-and-process)
                    (mark modified read-only vc-status-mini " "
                          (name 18 18 :left :elide)
                          " "
                          (size-h 9 -1 :right)
                          " "
                          (mode 16 16 :left :elide)
                          " "
                          (vc-status 16 16 :left)
                          " "
                          filename-and-process))
                  ibuffer-filter-group-name-face 'font-lock-doc-face)
            (define-ibuffer-column size-h
              (:name "Size" :inline t)
              (cond
               ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
               ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
               (t (format "%8d" (buffer-size)))))
            (defun ibuffer-set-up-preferred-filters ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'filename/process)
                (ibuffer-do-sort-by-filename/process)))
            (after-load 'ibuffer
              (fullframe ibuffer ibuffer-quit)))
  :bind ("C-x C-b" . ibuffer)
  :ensure t)

(use-package flycheck
  :ensure t
  :config (progn
            (add-hook 'after-init-hook 'global-flycheck-mode)
            (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
                  flycheck-idle-change-delay 0.8
                  flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)))

(use-package js
  :config (progn
            (setq js-indent-level 4))
  :ensure t)

(use-package imenu-anywhere
  :config (progn
            (after-load 'imenu-anywhere (global-set-key (kbd "C-.") 'imenu-anywhere))
            (bind-key "C-x i" 'imenu-anywhere))
  :ensure t)

(use-package ido
  :config (progn
            (ido-mode)
            (ido-vertical-mode)
            (ido-everywhere)
            (ido-ubiquitous-mode)
            (setq ido-enable-flex-matching t
                  ido-use-filename-at-point nil
                  ido-auto-merge-work-directories-length 0
                  ido-use-virtual-buffers t
                  smex-save-file (concat emacs-persistence-directory ".smex-items")
                  ido-default-buffer-method 'selected-window
                  ido-save-directory-list-file (concat emacs-persistence-directory ".ido-last"))
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
            (add-hook 'ido-setup-hook #'bind-ido-keys))
  :ensure imenu-anywhere
  :ensure ido-vertical-mode
  :ensure idomenu
  :ensure ido-completing-read+
  :ensure ido-ubiquitous
  :ensure smex)

(require 'init-hippie-expand)

(use-package switch-window
  :config (progn
            (setq switch-window-shortcut-style 'alphabet)
            ;;----------------------------------------------------------------------------
            ;; When splitting window, show (other-buffer) in the new window
            ;;----------------------------------------------------------------------------
            (defun split-window-func-with-other-buffer (split-function)
              (lexical-let ((s-f split-function))
                (lambda ()
                  (interactive)
                  (funcall s-f)
                  (set-window-buffer (next-window) (other-buffer)))))
            ;;----------------------------------------------------------------------------
            ;; Rearrange split windows
            ;;----------------------------------------------------------------------------
            (defun split-window-horizontally-instead ()
              (interactive)
              (save-excursion
                (delete-other-windows)
                (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))
            (defun split-window-vertically-instead ()
              (interactive)
              (save-excursion
                (delete-other-windows)
                (funcall (split-window-func-with-other-buffer 'split-window-vertically))))
            (defun my-split-vertically ()
              (interactive)
              (funcall (split-window-func-with-other-buffer 'split-window-vertically)))
            (defun my-split-horizontally ()
              (interactive)
              (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))
  :bind (("C-x o" . switch-window)
         ("C-x 1" . delete-other-windows)
         ("C-x 2" . my-split-vertically)
         ("C-x 3" . my-split-horizontally)
         ("\C-x|" . split-window-horizontally-instead)
         ("\C-x_" . split-window-vertically-instead)))

;; (require 'init-sessions)
(require 'init-fonts)
(require 'init-editing-utils)
(require 'init-darcs)
(require 'init-csv)
(require 'init-javascript)
(require 'init-web)
(require 'init-org)
(require 'init-nxml)
(require 'init-haml)
(require 'init-paredit)
(require 'init-lisp)
(require 'init-misc)

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

;; Extra packages which don't require any configuration

(use-package multiple-cursors
  :commands (mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package regex-tool
  :commands regex-tool)

(use-package pdf-tools
  :commands pdf-tools-install)

(use-package hl-line+
  :config (progn
            (set-face-background hl-line-face "#363636")))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package hackernews
  :commands hackernews)

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)))

(use-package dizzee
  :commands (dz-defservice dz-defservice-group))

(use-package list-processes+
  :commands list-processes+)

(use-package symon
  :commands symon-mode)

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
            (use-package github-clone
              :ensure t)
            (use-package yagist
              :ensure t)
            (use-package github-browse-file
              :ensure t)
            (use-package bug-reference-github
              :ensure t)
            (use-package magit-gh-pulls
              :ensure t))
  :bind (("C-x g s" . magit-status)
         ("C-x g b" . magit-blame)
         ("C-x g l" . magit-log-buffer-file)
         ("C-x g c" . magit-commit)
         ("C-x g p c" . magit-push-current)))

(use-package twittering-mode)

(use-package term+
  :config
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

(use-package itail)

(use-package frame-cmds)

(use-package goto-chg
  :bind (("C-c b ," . goto-last-change)
         ("C-c b ." . goto-last-change-reverse)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;----------------------------------------------------------------------------
;; User interface
;;----------------------------------------------------------------------------

(use-package smart-mode-line
  :defer t
  :config (progn
            (setq-default
             mode-line-format
             '("%e"
               mode-line-front-space
               mode-line-mule-info
               mode-line-client
               mode-line-modified
               mode-line-remote
               mode-line-frame-identification
               mode-line-buffer-identification
               "   "
               mode-line-position
               (vc-mode vc-mode)
               "  "
               mode-line-modes
               mode-line-misc-info
               mode-line-end-spaces))))

(use-package swiper
  :commands swiper
  :bind ("C-c s" . swiper)
  :ensure t)

(use-package string-edit
  :commands string-edit)

(use-package highlight-leading-spaces
  :init (add-hook 'prog-mode-hook 'highlight-leading-spaces-mode)
  :ensure t)

;; (use-package projectile
;;   :config (progn
;;             (setq projectile-indexing-method 'native
;;                   projectile-enable-caching t
;;                   projectile-file-exists-remote-cache-expire (* 10 60)
;;                   projectile-require-project-root nil)
;;             (projectile-global-mode))
;;   :ensure t)

(use-package codesearch
  :ensure t)

(use-package prosjekt
  :bind (("C-x y p o r e" . prosjekt-open-recent))
  :ensure t)

(use-package elscreen
  :config (progn
            (elscreen-start)
            (setq elscreen-display-tab nil))
  :ensure t)

(use-package super-save
  :config (progn
            (super-save-initialize))
  :ensure t)

(use-package clean-buffers
  :config (progn
            (defvar my-useless-buffer-names '("*Compile-Log*" "*Pp Eval Output*"))
            (setq clean-buffers-useless-buffer-names (append clean-buffers-useless-buffer-names my-useless-buffer-names)))
  :ensure t)

(use-package lice
  :commands lice
  :ensure t)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)
(mapc (lambda (x)
        (add-to-list 'completion-ignored-extensions x))
      '(".$$$" ".000" ".a" ".a26" ".a78" ".acn" ".acr" ".agdai" ".aif" ".alg" ".ali" ".aliases" ".annot" ".ap_" ".api" ".api-txt" ".apk" ".app" ".aps" ".autosave" ".aux" ".auxlock" ".avi" ".azurePubxml" ".bak" ".bbl" ".bcf" ".bck" ".beam" ".beams" ".bim.layout" ".bin" ".blg" ".booproj" ".bowerrc" ".box" ".bpi" ".bpl" ".brf" ".bs" ".build.csdef" ".byte" ".cachefile" ".c_date" ".cfg" ".cfgc" ".cgo1.go" ".cgo2.c" ".chi" ".chs.h" ".class" ".cma" ".cmi" ".cmo" ".cmp" ".cmx" ".cmxa" ".cmxs" ".crc" ".crs" ".csproj" ".css.map" ".cubin" ".d" ".dart.js" ".db" ".dbmdl" ".dbproj.schemaview" ".dcp" ".dcu" ".debug" ".debug.app" ".def" ".DEPLOYED" ".dex" ".dll" ".dmb" ".dotCover" ".DotSettings.user" ".dox" ".dpth" ".drc" ".drd" ".dres" ".dri" ".drl" ".dsk" ".dump" ".dvi" ".dylib" ".dyn_hi" ".dyn_o" ".ear" ".pyc"))

;; https://github.com/tkf/emacs-request
;; (use-package emacs-request)

(load-theme 'zerodark t)
(require 'init-gui-frames)
;; (require 'ej-autocomplete)

(require 'init-local nil t)
(provide 'init)

;;; init.el ends here
