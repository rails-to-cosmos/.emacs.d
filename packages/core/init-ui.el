;;; init-ui.el --- my UI settings
;;; Commentary:
;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(set-cursor-color "#D0E1F9")
(blink-cursor-mode 0)

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
                :height 100
                :width normal
                :foundry nil
                :family "Menlo")))))

(use-package danneskjold-theme
  :ensure t)

(defun what-face (pos)
  "Tell me what face used in POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

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
              inhibit-startup-screen t
              line-spacing 7
              visible-bell nil
              cursor-type 'box
              frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))) buffers-menu-max-size 30)

(provide 'init-ui)
;;; init-ui.el ends here
