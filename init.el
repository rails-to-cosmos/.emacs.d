;;; init.el --- Dmitry Akatov's emacs configuration
;;
;; Filename: init.el
;; Description: Dmitry Akatov's emacs configuration
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


;;----------------------------------------------------------------------------
;; Init use-package
;;----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))
(use-package diminish)
(use-package bind-key)

;;----------------------------------------------------------------------------
;; Paths
;;----------------------------------------------------------------------------

(setq emacs-persistence-directory (concat user-emacs-directory "persistence/")
      savehist-file (concat emacs-persistence-directory ".minibuffer-history")
      ac-comphist-file (concat emacs-persistence-directory ".ac-comphist")
      session-save-file (concat emacs-persistence-directory ".session")
      frame-restore-parameters-file (concat emacs-persistence-directory ".frame-restore-parameters")
      bmkp-bmenu-state-file (concat emacs-persistence-directory ".emacs-bmk-bmenu-state")
      bookmark-default-file (concat emacs-persistence-directory ".bookmarks")
      desktop-path (list emacs-persistence-directory)
      recentf-save-file (concat emacs-persistence-directory ".recentf")
      custom-file (concat emacs-persistence-directory ".custom")
      eshell-directory-name (concat emacs-persistence-directory "eshell")
      mc/list-file (concat emacs-persistence-directory ".mc-lists")
      abbjrev-file-name (concat emacs-persistence-directory ".abbrev-defs"))
(make-directory emacs-persistence-directory t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


;;----------------------------------------------------------------------------
;; Init shell
;;----------------------------------------------------------------------------

(require 'init-shell)

(defun eshell-init-aliases()
  (add-to-list 'eshell-command-aliases-list '("l" "ls"))
  (add-to-list 'eshell-command-aliases-list '("ll" "ls -la"))
  (add-to-list 'eshell-command-aliases-list '("pip-update" "pip freeze --local | grep -v '^\\-e' | cut -d = -f 1  | xargs -n1 pip install -U")))

(add-hook 'eshell-mode-hook 'eshell-init-aliases)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (setenv "LANG" "en_US.UTF-8")
    (setenv "LC_ALL" "en_US.UTF-8")
    (setenv "LC_CTYPE" "en_US.UTF-8")))


;;----------------------------------------------------------------------------
;; System constants
;;----------------------------------------------------------------------------

(defconst *is-a-mac* (eq system-type 'darwin))
(setq mac-command-modifier 'meta)


;;----------------------------------------------------------------------------
;; User interface
;;----------------------------------------------------------------------------

;; (defun set-frame-size-according-to-resolution ()
;;   (interactive)
;;   (if window-system
;;   (progn
;;     ;; use 120 char wide window for largeish displays
;;     ;; and smaller 80 column windows for smaller displays
;;     ;; pick whatever numbers make sense for you
;;     (if (> (x-display-pixel-width) 1280)
;;            (add-to-list 'default-frame-alist (cons 'width 160)))
;;     ;; for the height, subtract a couple hundred pixels
;;     ;; from the screen height (for panels, menubars and
;;     ;; whatnot), then divide by the height of a char to
;;     ;; get the height we want
;;     (add-to-list 'default-frame-alist
;;          (cons 'height (/ (- (x-display-pixel-height) 80)
;;                              (frame-char-height)))))))

;; (when window-system
;;   (set-frame-size-according-to-resolution))

(require 'init-gui-frames)

(use-package smart-mode-line
  :defer t
  :config
  (progn
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

(use-package miniedit
  :defer t
  :ensure t
  :commands minibuffer-edit
  :init (miniedit-install))

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

(use-package wgrep)


;;----------------------------------------------------------------------------
;; Logging
;;----------------------------------------------------------------------------

(use-package mwe-log-commands)


;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(use-package bookmark+)
(use-package scratch)
(use-package yasnippet
  :config
  (progn
    (yas-global-mode 1)
    (bind-key "C-j" 'yas-expand yas-minor-mode-map)))
(use-package emmet-mode)
(use-package impatient-mode)
(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))
;; (use-package emacsql
;;   :ensure pg)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("ipython" . python-mode)
  :load-path "python/"
  :config
  (progn
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
    ;; (make-directory "~/.virtualenvs" t)
    (jedi:install-server))
  :bind (("C-c C-b" . python-add-breakpoint))
  ;; :ensure virtualenv
  :ensure jedi
  :ensure cinspect
  :ensure py-isort
  :ensure py-yapf
  :ensure pyenv-mode
  :ensure pyvenv
  :ensure pungi
  :ensure yasnippet)

(use-package live-py-mode)

(require 'init-dired)
(require 'init-isearch)

(use-package uniquify
  :disabled t
  :init
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package ibuffer
  :config
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
    (fullframe ibuffer ibuffer-quit))
  :bind ("C-x C-b" . ibuffer)
  :ensure fullframe
  :ensure ibuffer-vc
  :ensure ibuffer-git)

(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package js
  :config
  (setq js-indent-level 4))

(use-package imenu-anywhere
  :config
  (progn
    (after-load 'imenu-anywhere (global-set-key (kbd "C-.") 'imenu-anywhere))
    (bind-key "C-x i" 'imenu-anywhere)))


(use-package ido
  :config
  (progn
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
  :config
  (progn
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
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package regex-tool)

(use-package pdf-tools)

(use-package hl-line+
  :config (set-face-background hl-line-face "#363636"))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package hackernews)

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)))

(use-package dizzee)
(use-package list-processes+)

(use-package symon)

(use-package camcorder
  :commands camcorder-mode)

(use-package nhexl-mode)

;; https://github.com/kiwanami/emacs-calfw
(use-package calfw
  :config
  (require 'calfw-org))

(use-package keyfreq
  :config
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

(when (eval-when-compile (>= emacs-major-version 24.4))
  (use-package magit
    :bind (("C-x g s" . magit-status)
	   ("C-x g b" . magit-blame)
	   ("C-x g l" . magit-log-buffer-file)
	   ("C-x g c" . magit-commit)
	   ("C-x g p c" . magit-push-current))
    :commands magit-status
    :ensure git-gutter+
    :ensure github-clone
    :ensure yagist
    :ensure github-browse-file
    :ensure bug-reference-github
    :ensure magit-gh-pulls)

  (use-package git-gutter+
    :config (progn
	      (global-git-gutter+-mode))))

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

(use-package elscreen
  :config (progn
            (elscreen-start)
            (setq elscreen-display-tab nil)))

;; https://github.com/tkf/emacs-request
;; (use-package emacs-request)

(load-theme 'zerodark t)
(require 'init-gui-frames)
;; (require 'ej-autocomplete)

(require 'init-local nil t)
(provide 'init)

;;; init.el ends here
