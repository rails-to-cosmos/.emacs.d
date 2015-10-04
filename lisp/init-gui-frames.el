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


;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------

(setq scroll-conservatively 50
      scroll-margin 4
      inhibit-splash-screen t
      inhibit-startup-message t
      save-abbrevs t
      use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      indicate-empty-lines t)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(column-number-mode t)
(delete-selection-mode t)
(fringe-mode '(10 . 0))
(global-visual-line-mode 1)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------

(mapc (lambda (mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode menu-bar-mode scroll-bar-mode))

(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      indent-tabs-mode nil)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun sanityinc/adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "M-C-8") '(lambda () (interactive) (sanityinc/adjust-opacity nil -10)))
(global-set-key (kbd "M-C-9") '(lambda () (interactive) (sanityinc/adjust-opacity nil 10)))
(global-set-key (kbd "M-C-0") '(lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))


(provide 'init-gui-frames)
