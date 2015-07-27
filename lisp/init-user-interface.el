;;; Package --- init-user-interface

;;; Commentary:
;;; Default user interface settings

;;; Code:

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

(use-package emmet-mode
  :ensure t)

(use-package elmacro
  :ensure t)

(defun make-frame-transparent ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 85)))

(defun make-frame-opaque ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

;;; TODO optimize it:
(load-theme 'zerodark t)
(provide 'init-user-interface)

;;; init-user-interface.el ends here
