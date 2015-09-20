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
;; Use-package initialization
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
      smex-save-file (concat emacs-persistence-directory ".smex-items")
      session-save-file (concat emacs-persistence-directory ".session")
      frame-restore-parameters-file (concat emacs-persistence-directory ".frame-restore-parameters")
      bmkp-bmenu-state-file (concat emacs-persistence-directory ".emacs-bmk-bmenu-state")
      bookmark-default-file (concat emacs-persistence-directory ".bookmarks")
      desktop-path (list emacs-persistence-directory)
      ido-save-directory-list-file (concat emacs-persistence-directory ".ido-last")
      custom-file (concat emacs-persistence-directory ".custom")
      eshell-directory-name (concat emacs-persistence-directory "eshell")
      mc/list-file (concat emacs-persistence-directory ".mc-lists")
      abbjrev-file-name (concat emacs-persistence-directory ".abbrev-defs"))
(make-directory emacs-persistence-directory t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;----------------------------------------------------------------------------
;; Env vars
;;----------------------------------------------------------------------------

;;(setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH"))

(let ((path-from-shell (replace-regexp-in-string "^.*\n.*shell\n" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;;----------------------------------------------------------------------------
;; System constants
;;----------------------------------------------------------------------------

(defconst *is-a-mac* (eq system-type 'darwin))
(setq mac-command-modifier 'meta)

;;----------------------------------------------------------------------------
;; User interface
;;----------------------------------------------------------------------------
(load-theme 'zerodark t)

;;----------------------------------------------------------------------------
;; Text editing utils
;;----------------------------------------------------------------------------

(require 'init-utils)

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
(use-package yasnippet)
(use-package emmet-mode)
(use-package impatient-mode)

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("ipython" . python-mode)
  :load-path "python/"
  :config
  (defun python-add-breakpoint ()
    "Add a break point"
    (interactive)
    (insert "import ipdb; ipdb.set_trace()")
    (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
  (setq jedi:complete-on-dot t)
  (eval-after-load "python"
    '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
  (add-hook 'python-mode-hook 'linum-mode)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'jedi-mode-hook 'jedi-direx:setup)
  :bind (("C-c C-b" . python-add-breakpoint))
  :ensure jedi
  :ensure cinspect
  :ensure jedi-direx
  :ensure py-isort
  :ensure py-yapf
  :ensure pyenv-mode
  :ensure virtualenvwrapper
  :ensure py-autopep8
  :ensure ob-ipython)

(require 'init-gui-frames)
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

(use-package ido
  :config
  (ido-mode t)
  (ido-vertical-mode t)
  (ido-everywhere t)
  (ido-ubiquitous-mode t)
  (setq ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-auto-merge-work-directories-length 0
        ido-use-virtual-buffers t
        smex-save-file (expand-file-name ".smex-items" user-emacs-directory)
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
    (define-key ido-completion-map (kbd "C-p")   'ido-prev-match))
  (add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))
  (add-hook 'ido-setup-hook #'bind-ido-keys)
  :ensure ido-vertical-mode
  :ensure idomenu
  :ensure ido-completing-read+
  :ensure ido-ubiquitous
  :ensure smex)

(require 'init-hippie-expand)

(use-package switch-window
  :config
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
  (global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
  (global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))
  (global-set-key "\C-x1" 'delete-other-windows)
  :bind (("C-x o" . switch-window)
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

(use-package imenu-anywhere
  :bind (("C-." . imenu-anywhere)))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(require 'init-shell)
(require 'init-local)

(use-package hackernews)

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)))

(use-package dizzee)

(use-package list-processes+)

(use-package symon
  :config (symon-mode t))

(use-package camcorder
  :commands camcorder-mode)



(provide 'init)

;;; init.el ends here
