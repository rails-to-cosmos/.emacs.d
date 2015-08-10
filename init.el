;;; init.el --- Dmitry Akatov's emacs configuration
;;
;; Filename: init.el
;; Description: Dmitry Akatov's emacs configuration
;; Author: Dmitry Akatov
;; Created: Sun Aug 09 21:49:00 2015 (-0400)
;; Version: 1.0.0
;; URL: www.github.com/jordonbiondo/.emacs.d
;; Keywords: Emacs 24.3
;; Compatibility: emacs >= 24.3
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

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(package-refresh-contents)

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

      ;; shell-pop-default-directory (concat emacs-persistence-directory "shell-pop")
      ;; shell-pop-shell-type (quote ("ansi-term" "*pop-shell*" (lambda nil (ansi-term shell-pop-term-shell))))
      ;; shell-pop-term-shell "/bin/bash"
      ;; shell-pop-universal-key "C-t"
      ;; shell-pop-window-size 30
      ;; shell-pop-full-span t
      ;; shell-pop-window-position "bottom"

(make-directory emacs-persistence-directory t)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH"))

(defconst *is-a-mac* (eq system-type 'darwin))
(setq mac-command-modifier 'meta)

(defconst *is-a-win* (eq system-type 'windows-nt))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------

(use-package init-utils)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(use-package gnuplot
  :ensure t)
(use-package dsvn
  :ensure t)
(use-package bookmark+
  :ensure t)
(use-package wgrep
  :ensure t)
(use-package diminish
  :ensure t)
(use-package scratch
  :ensure t)
(use-package mwe-log-commands
  :ensure t)
(use-package restclient
  :ensure t
  :config
  (add-auto-mode 'restclient-mode "\\.rest\\'"))
(use-package yasnippet
  :ensure t)
(use-package emmet-mode
  :ensure t)
(use-package impatient-mode
  :ensure t)
;; http://localhost:8080/imp/
(use-package python-django
  :ensure t)


(use-package init-frame-hooks)
(use-package init-xterm)
(use-package init-osx-keys)
(use-package init-gui-frames)
(use-package init-proxies)
(use-package init-dired)
(use-package init-isearch)
(use-package init-uniquify)
(use-package init-ibuffer)
(use-package init-flycheck)
(use-package init-recentf)
(use-package init-ido)
(use-package init-hippie-expand)
(use-package init-auto-complete)
(use-package init-windows)
(use-package init-sessions)
(use-package init-fonts)
(use-package init-editing-utils)
(use-package init-darcs)
(use-package init-foldings)
(use-package init-crontab)
(use-package init-textile)
(use-package init-markdown)
(use-package init-csv)
(use-package init-javascript)
(use-package init-web)
(use-package init-org)
(use-package init-nxml)
(use-package init-haml)
(use-package init-python-mode)
(use-package init-paredit)
(use-package init-lisp)
(use-package init-spelling)
(use-package init-misc)

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(use-package init-locales)

;; Extra packages which don't require any configuration

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package regex-tool
  :ensure t)

(use-package pdf-tools
  :ensure t)

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(asm-mode
                                     c++-mode
                                     c-mode
                                     clojure-mode
                                     cperl-mode
                                     csharp-mode
                                     espresso-mode
                                     factor-mode
                                     haskell-mode
                                     js-mode
                                     latex-mode
                                     lisp-mode
                                     lua-mode
                                     nxml-mode
                                     objc-mode
                                     php-mode
                                     plain-tex-mode
                                     python-mode
                                     rspec-mode
                                     ruby-mode
                                     rust-mode
                                     scheme-mode
                                     vbnet-mode
                                     emacs-lisp-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(use-package hl-line+
  :config (set-face-background hl-line-face "#363636")
  :ensure t)

(use-package imenu-anywhere
  :bind (("C-." . imenu-anywhere))
  :ensure t)

(use-package fiplr
  :bind (("C-x f" . fiplr-find-file))
  :ensure t)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package init-shell)
(use-package init-local)
(use-package hackernews
  :ensure t)

;;----------------------------------------------------------------------------
;; User interface
;;----------------------------------------------------------------------------

(setq-default
 buffers-menu-max-size 30
 case-fold-search t
 compilation-scroll-output t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell t
 line-spacing 5
 indent-tabs-mode nil)

(setq scroll-conservatively 50
      scroll-margin 4
      inhibit-splash-screen t
      inhibit-startup-message t
      save-abbrevs t)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(column-number-mode t)
(delete-selection-mode t)
(tool-bar-mode -1)
(fringe-mode '(10 . 0))
(global-visual-line-mode 1)

(defun make-frame-transparent ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 85)))

(defun make-frame-opaque ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

;;; TODO optimize it:
(load-theme 'zerodark t)

(provide 'init)
;;; init.el ends here
