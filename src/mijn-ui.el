;; -*- lexical-binding: t; -*-
;; These chrome toggles are undefined in builds without the corresponding
;; support (terminal/headless), so guard each one.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'grab-and-drag)
(require 'frame)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(use-package default-text-scale
  :ensure nil)

(use-package highlight
  :ensure nil)

(use-package ace-window
  :config (setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i)
                aw-scope 'frame)
  :bind (("C-x C-o" . ace-window)
         ("M-o" . ace-window))
  :ensure nil)

(cl-defun my:codegen-current-frame-settings ()
  (interactive)
  (let* ((frame (selected-frame))
         (frame-geometry (frame-geometry))
         (_frame-x (car frame-geometry))
         (_frame-y (cadr frame-geometry))
         (font (face-attribute 'default :font frame))
         (font-info (font-info font))
         (font-name (aref font-info 0))
         (_font-size (face-attribute 'default :height frame)))
    (cl-destructuring-bind (frame-x . frame-y) (cdar (frame-geometry))
      (insert (s-join "\n"
                      (list (format "(set-frame-font \"%s\" nil t)" font-name)
                            (format "(set-frame-size (selected-frame) %d %d)" (frame-width) (frame-height))
                            (format "(set-frame-position (selected-frame) %d %d)" frame-x frame-y)))))))

(cl-defun my:toggle-no-other-window ()
  "Toggle the `no-other-window' parameter for the current window."
  (interactive)
  (let* ((win (selected-window))
         (current (window-parameter win 'no-other-window)))
    (set-window-parameter win 'no-other-window (not current))
    (message "Window is now %s for `other-window`" (if (not current) "SKIPPED" "SELECTABLE"))))

(require 'mijn-fonts)

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
              line-spacing 7)

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

;; Cap automatic popups (display-buffer) at two windows: when there are
;; already 2+, return nil so display-buffer reuses an existing window
;; instead of carving out a third. Manual C-x 2 / C-x 3 still work.
(setq split-window-preferred-function
      (lambda (window)
        (when (< (length (window-list)) 2)
          (split-window-sensibly window))))

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
    :ensure nil)

(use-package rainbow-delimiters
    :ensure nil)

;; Only enable if the package is actually provided.  A void function in
;; `prog-mode-hook' otherwise fires during every package byte-compile and
;; aborts the install.
(when (fboundp 'rainbow-mode)
  (add-hook 'prog-mode-hook #'rainbow-mode))
(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(grab-and-drag-mode 1)
(setq grab-and-drag-pointer-shape nil)

(defun mijn-theme-variant ()
  "Return the durable desktop theme variant, the symbol `light' or `dark'.
Reads ~/.config/xmobar/theme-variant, the single source of truth shared
with xmobar and its status daemon.  Defaults to `dark' when unset."
  (let ((f (expand-file-name "~/.config/xmobar/theme-variant")))
    (if (and (file-readable-p f)
             (with-temp-buffer
               (insert-file-contents f)
               (string-match-p "light" (buffer-string))))
        'light
      'dark)))

(defun mijn-apply-emacs-theme (variant)
  "Enable the danneskjold theme matching VARIANT (`light' or `dark')."
  (if (eq variant 'light)
      (progn (disable-theme 'danneskjold)
             (enable-theme 'danneskjold-light))
    (disable-theme 'danneskjold-light)
    (enable-theme 'danneskjold)))

(defun mijn-current-emacs-variant ()
  "Return the variant currently active in Emacs, `light' or `dark'."
  (if (custom-theme-enabled-p 'danneskjold-light) 'light 'dark))

(defun mijn-sync-emacs-theme (&rest _)
  "Re-apply the Emacs theme to match the durable variant if it drifted.
Idempotent: a no-op when Emacs already shows the desktop variant, so it
is safe to call from a file-watch or a timer (and it will not loop with
Emacs's own toggle, which applies the variant before writing the file)."
  (let ((want (mijn-theme-variant)))
    (unless (eq want (mijn-current-emacs-variant))
      (mijn-apply-emacs-theme want)
      (message "Theme synced to %s (external change)" want))))

(defvar mijn-theme-poll-timer nil
  "Repeating timer that follows the durable theme variant.")

(defun mijn-watch-theme-variant ()
  "Sensor: poll the durable theme-variant and sync Emacs when it drifts.
A timer, not file-notify: inotify callbacks are not delivered reliably
in a headless Emacs daemon, and the check is cheap (one small file read
plus a symbol compare)."
  (when mijn-theme-poll-timer (cancel-timer mijn-theme-poll-timer))
  (setq mijn-theme-poll-timer (run-at-time 2 2 #'mijn-sync-emacs-theme)))

(use-package danneskjold-theme
  ;; Start in whatever variant the durable knob says, so Emacs matches the bar
  ;; on a fresh session instead of always coming up dark, then keep following it.
  :config
  (mijn-apply-emacs-theme (mijn-theme-variant))
  (mijn-watch-theme-variant)
  :bind (("C-x y t t" . #'danneskjold-toggle-theme))
  :ensure nil)

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

(defun xmobar-toggle-theme ()
  "Flip the durable desktop theme (Emacs + xmobar) between light and dark.
Reads the current variant from the shared source of truth, applies the
opposite here, and hands it to theme-sync.sh, which persists it and
recolors the bar."
  (interactive)
  (let ((new (if (eq (mijn-theme-variant) 'light) 'dark 'light)))
    (mijn-apply-emacs-theme new)
    (message "Switched to %s theme" new)
    (start-process "xmobar-theme-sync" nil "bash"
                   (expand-file-name "~/.config/xmonad/scripts/theme-sync.sh")
                   (symbol-name new))))

(global-set-key (kbd "C-x y t x") #'xmobar-toggle-theme)

(provide 'mijn-ui)
