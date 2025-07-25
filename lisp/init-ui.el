(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'grab-and-drag)

(when (> (string-to-number emacs-version) 30)
  (pixel-scroll-precision-mode))

(use-package default-text-scale
  :ensure t)

(use-package highlight
  :ensure t)

(use-package ace-window
  :config (setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i)
                aw-scope 'frame)
  :bind (("C-x C-o" . ace-window)
         ("M-o" . ace-window))
  :ensure t)

(cl-defun my:codegen-current-frame-settings ()
  (interactive)
  (let* ((frame (selected-frame))
         (frame-geometry (frame-geometry))
         (frame-x (car frame-geometry))
         (frame-y (cadr frame-geometry))
         (font (face-attribute 'default :font frame))
         (font-info (font-info font))
         (font-name (aref font-info 0))
         (font-size (face-attribute 'default :height frame)))
    (cl-destructuring-bind (frame-x . frame-y) (cdar (frame-geometry))
      (insert (s-join "\n"
                      (list (format "(set-frame-font \"%s\" nil t)" font-name)
                            (format "(set-frame-size (selected-frame) %d %d)" (frame-width) (frame-height))
                            (format "(set-frame-position (selected-frame) %d %d)" frame-x frame-y)))))))

(cl-defun my:toggle-no-other-window ()
  "Toggle the 'no-other-window' parameter for the current window."
  (interactive)
  (let* ((win (selected-window))
         (current (window-parameter win 'no-other-window)))
    (set-window-parameter win 'no-other-window (not current))
    (message "Window is now %s for `other-window`" (if (not current) "SKIPPED" "SELECTABLE"))))

(set-frame-font "-JB-JetBrains Mono NL-regular-normal-normal-*-11-*-*-*-m-0-iso10646-1" t nil)

(setq-default ring-bell-function 'ignore
              use-dialog-box nil
              use-file-dialog nil
              visible-bell nil
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
              tooltip-mode nil
              tooltip-delay 1.5
              tooltip-use-echo-area t
              x-gtk-use-system-tooltips nil
              truncate-lines nil
              truncate-partial-width-windows nil
              indent-tabs-mode nil
              x-use-underline-position-properties t
              underline-minimum-offset 3
              inhibit-startup-screen t
              cursor-type 'box
              split-width-threshold 160
              split-height-threshold nil
              frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))
              buffers-menu-max-size 30
              line-spacing 7
              initial-scratch-message "# I've always thought they were lighthouses\n\n")

(defun what-face (pos)
  "Tell me what face used in POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun split-window-next-buffer ()
  (interactive)
  (let ((window (split-window-right)))
    (select-window window)
    (switch-to-buffer (cl-loop for buffer in (cdr (buffer-list (selected-frame)))
                               unless (minibufferp buffer)
                               return buffer))))

(global-unset-key (kbd "C-x 3"))
(global-set-key (kbd "C-x 3") #'split-window-next-buffer)

(transient-mark-mode)

(setq comint-prompt-read-only t)
(add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)

(define-key global-map (kbd "RET") 'newline-and-indent)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; http://pragmaticemacs.com/emacs/make-emacs-a-bit-quieter/
(advice-add #'display-startup-echo-area-message :override #'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(blink-cursor-mode 0)

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (user-error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(use-package rainbow-mode
    :ensure t)

(use-package rainbow-delimiters
    :ensure t)

(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(grab-and-drag-mode 1)
(setq grab-and-drag-pointer-shape nil)

(use-package danneskjold-theme
  :config (enable-theme 'danneskjold)
  :bind (("C-x y t t" . #'danneskjold-toggle-theme))
  :ensure t)

(eval-after-load 'org-faces
  '(defcustom org-todo-keyword-faces nil
    "Faces for specific TODO keywords.
This is a list of cons cells, with TODO keywords in the car and
faces in the cdr.  The face can be a symbol, a color, or a
property list of attributes, like (:foreground \"blue\" :weight
bold :underline t)."
    :group 'org-faces
    :group 'org-todo
    :type '(repeat
            (cons
             (string :tag "Keyword")
             (choice color (sexp :tag "Face"))))))

(eval-after-load 'org
  '(progn
    (defun org-get-todo-face-from-color (color)
      "Returns a specification for a face that inherits from org-todo
 face and has the given color as foreground. Returns nil if
 color is nil."
      (when color
        `(:inherit org-warning :foreground ,color)))

    (defun org-get-todo-face (kwd)
      "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
      (if (numberp kwd) (setq kwd (match-string kwd)))
      (or (let ((face (cdr (assoc kwd org-todo-keyword-faces))))
            (if (stringp face)
                (org-get-todo-face-from-color face)
              face))
          (and (member kwd org-done-keywords) 'org-done)
          'org-todo))

    (setq org-todo-keyword-faces
     '(("REVIEW" . (:foreground "#292"))
       ("CANCELLED" . (:foreground "#7B9ED2"))
       ("STARTED" . (:foreground "#749AF7"))
       ("PENDING" . (:foreground "#e67e22")) ;FFDB45
       ("DELEGATED" . (:foreground "DarkCyan"
                       :slant italic
                       ;; :box (:line-width 3 :color "#FFDB45")
                       ))
       ("CODE_REVIEW" . (:foreground "#8B7FD7"))))))

;; (require 'color)
;; (set-face-attribute 'org-block nil :background
;;                     (color-darken-name
;;                      (face-attribute 'default :background) 3))

;; (setq org-src-block-faces '(("emacs-lisp" (:background "#EEE2FF"))
;;                             ("python" (:background "#E5FFB8"))))

;; )

(provide 'init-ui)
