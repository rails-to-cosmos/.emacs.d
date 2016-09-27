(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

(use-package my/cursor
  :no-require t
  :init (progn
          (setq-default cursor-type 'box)
          (set-cursor-color "#D0E1F9")
          (blink-cursor-mode 0)))

(use-package my/alarm-bell
  :init (progn
          (setq visible-bell nil)))

(use-package my/frame-interface
  :no-require t
  :init (progn
          (setq-default frame-title-format '((:eval (if (buffer-file-name)
                                                        (abbreviate-file-name (buffer-file-name))
                                                      "%b")))
                        buffers-menu-max-size 30)))

(use-package my/font
  :no-require t
  :init (progn
          (setq-default line-spacing 7)

          (custom-set-faces
           '(default ((t (:inherit nil
                                   :stipple nil
                                   :inverse-video nil
                                   :box nil
                                   :strike-through nil
                                   :overline nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :height 120
                                   :width normal
                                   :foundry nil
                                   :family "Menlo")))))))

(use-package my/themes
  :no-require t
  :init (progn
          (use-package danneskjold-theme
            :ensure t)))

(setq-default case-fold-search t
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
              show-trailing-whitespace nil
              tooltip-delay 1.5
              truncate-lines nil
              truncate-partial-width-windows nil
              indent-tabs-mode nil
              x-use-underline-position-properties t
              underline-minimum-offset 3
              inhibit-startup-screen t)

(use-package my/what-face
  :commands (what-face)
  :config (progn
            (defun what-face (pos)
              (interactive "d")
              (let ((face (or (get-char-property (point) 'read-face-name)
                              (get-char-property (point) 'face))))
                (if face (message "Face: %s" face) (message "No face at %d" pos))))))

(provide 'ui)
