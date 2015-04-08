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
      abbrev-file-name (concat emacs-persistence-directory ".abbrev-defs"))

(unless (file-exists-p emacs-persistence-directory)
  (make-directory emacs-persistence-directory t))
(unless (file-exists-p eshell-directory-name)
  (make-directory eshell-directory-name t))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------

(require 'init-utils)
(require 'init-elpa)      ;; Machinery for installing required packages

;;----------------------------------------------------------------------------
;; User interface
;;----------------------------------------------------------------------------
(tool-bar-mode -1)
(setq-default line-spacing 5)
(fringe-mode -1)
(global-visual-line-mode 1)
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(setq scroll-conservatively 50)
(setq scroll-margin 4)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(blink-cursor-mode -1)
(setq save-abbrevs t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(use-package wgrep)
(use-package diminish)
(use-package scratch)
(use-package mwe-log-commands)
(use-package restclient)
(use-package yasnippet)
(use-package emmet-mode)
(use-package python-django)
(use-package init-frame-hooks)
(use-package init-xterm)
(use-package init-themes)
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
(use-package init-vc)
(use-package init-darcs)
(use-package init-git)
(use-package init-foldings)
(use-package init-crontab)
(use-package init-textile)
(use-package init-markdown)
(use-package init-csv)
(use-package init-erlang)
(use-package init-javascript)
(use-package init-php)
(use-package init-org)
(use-package init-nxml)
(use-package init-haml)
(use-package init-python-mode)
(use-package init-paredit)
(use-package init-lisp)
(use-package init-spelling)
(use-package init-misc)
(use-package init-ledger)

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(use-package init-locales)

;; Extra packages which don't require any configuration
(use-package gnuplot)
(use-package dsvn)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package regex-tool)

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

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)

(use-package bookmark+)
(use-package dired+)
(use-package dedicated)

(use-package hl-line+
  :config (set-face-background hl-line-face "#363636"))

(use-package imenu-anywhere
  :bind (("C-." . imenu-anywhere)))

(use-package fiplr
  :bind (("C-x f" . fiplr-find-file)))

;; (use-package golden-ratio
;;   :config (golden-ratio-mode 1))

(defun make-frame-transparent ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 85)))

(defun make-frame-opaque ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

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

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
          (goto-char pos)
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) pos)
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))))
(global-set-key (kbd "C-c C-n C-n") 'unpop-to-mark-command)

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
(global-set-key (kbd "C-^") 'top-join-line)

(defun save-macro (name)
  "save a macro. Take a name as argument
        and save the last defined macro under
        this name at the end of your .emacs"
  (interactive "SName of the macro :") ; ask for the name of the macro
  (kmacro-name-last-macro name)        ; use this name for the macro
  (find-file user-init-file)   ; open ~/.emacs or other user init file
  (goto-char (point-max))      ; go to the end of the .emacs
  (newline)                    ; insert a newline
  (insert-kbd-macro name)      ; copy the macro
  (newline)                    ; insert a newline
  (switch-to-buffer nil))      ; return to the initial buffer
(global-set-key (kbd "C-x _") 'save-macro)

(use-package init-eshell-commands)

(provide 'init)
