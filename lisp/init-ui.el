(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'grab-and-drag)

(pixel-scroll-precision-mode)

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

(defun toggle-no-other-window ()
  "Toggle the 'no-other-window' parameter for the current window."
  (interactive)
  (let* ((win (selected-window))
         (current (window-parameter win 'no-other-window)))
    (set-window-parameter win 'no-other-window (not current))
    (message "Window is now %s for `other-window`" (if (not current) "SKIPPED" "SELECTABLE"))))


(defun my-limit-window-splitting (original-function &rest args)
  "Limit window splitting to two."
  (if (>= (length (window-list)) 2)
      nil ; If there are already two or more windows, do not split further
    (apply original-function args)))

(advice-add 'split-window-sensibly :around #'my-limit-window-splitting)

(setq-default my-font-name "JetBrains Mono NL-11"
              ring-bell-function 'ignore
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
              tooltip-delay 1.5
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

(defun my/apply-font ()
  (set-frame-font my-font-name nil t)
  (add-to-list 'default-frame-alist (cons 'font my-font-name)))

(add-hook 'after-init-hook #'my/apply-font)

(defun what-face (pos)
  "Tell me what face used in POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my/set-font (face &rest params)
  (custom-set-faces (list face (list (list t params)))))

(defun my/adapt-font (&optional font-size font-name)
  (interactive)
  (let ((font-size (or font-size (face-attribute 'default :height)))
        (font-name (or font-name (face-attribute 'default :family)))
        (factor (cl-case system-type
                  (darwin 10)
                  (windows-nt 5)
                  (t 0))))

    (my/set-font 'default
                 :slant 'normal
                 :weight 'normal
                 :height font-size
                 :width 'normal
                 :family font-name)

    (set-fontset-font "fontset-default"
                      (cons (decode-char 'ucs #x0400)
                            (decode-char 'ucs #x052F))
                      (if (> factor 0)
                          (font-spec :size (/ font-size factor) :name font-name :family font-name)
                        (font-spec :name font-name :family font-name)))))

(use-package default-text-scale
    :ensure t)

(define-key global-map (kbd "C-M-=") #'default-text-scale-increase)
(define-key global-map (kbd "C-M--") #'default-text-scale-decrease)
(default-text-scale-mode 1)
(advice-add #'default-text-scale-increase :after #'my/adapt-font)
(advice-add #'default-text-scale-decrease :after #'my/adapt-font)

(add-hook 'org-mode-hook #'my/adapt-font)

(defun split-window-next-buffer ()
  (interactive)
  (let ((window (split-window-right)))
    (select-window window)
    (switch-to-buffer (cl-loop for buffer in (cdr (buffer-list (selected-frame)))
                               unless (minibufferp buffer)
                               return buffer))))

(global-unset-key (kbd "C-x 3"))
(define-key global-map (kbd "C-x 3") (function split-window-next-buffer))

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
