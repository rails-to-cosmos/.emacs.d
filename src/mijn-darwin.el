;; -*- lexical-binding: t; -*-

(require 'json)
(require 'seq)

;; These are defined only in macOS Emacs builds; declare them so byte-compiling
;; on other platforms does not warn about free variables.
(defvar mac-command-modifier)
(defvar mac-right-command-modifier)
(defvar mac-option-modifier)
(defvar mac-redisplay-dont-reset-vscroll)
(defvar ns-use-native-fullscreen)
(defvar ns-pop-up-frames)
(defvar alert-default-style)

(defun mijn-darwin-caps-is-command-p (&optional file)
  "Whether FILE's selected Karabiner profile maps Caps Lock to left Command.
FILE defaults to Karabiner's configuration on this machine."
  (condition-case nil
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (json-false nil)
             (json-null nil)
             (config (json-read-file
                      (or file (expand-file-name "~/.config/karabiner/karabiner.json"))))
             (profile (seq-find (lambda (profile) (alist-get 'selected profile))
                                (alist-get 'profiles config))))
        (seq-some
         (lambda (mapping)
           (and (equal (alist-get 'key_code (alist-get 'from mapping)) "caps_lock")
                (equal (alist-get 'to mapping) '(((key_code . "left_command"))))))
         (alist-get 'simple_modifications profile)))
    (error nil)))

(when (eq system-type 'darwin)
  (let ((caps-is-command (mijn-darwin-caps-is-command-p)))
    (setq mac-command-modifier (if caps-is-command 'control 'meta)
          mac-right-command-modifier (if caps-is-command 'meta 'left)))
  (setq mac-option-modifier 'meta
        ;; sane trackpad/mouse scroll settings
        mac-redisplay-dont-reset-vscroll t
        ;; mac-mouse-wheel-smooth-scroll nil
        ;; mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
        ;; mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
        ;; Curse Lion and its sudden but inevitable fullscreen mode!
        ;; NOTE Meaningless to railwaycat's emacs-mac build
        ns-use-native-fullscreen t
        ;; Don't open files from the workspace in a new frame
        ns-pop-up-frames nil
        alert-default-style 'osx-notifier))

(provide 'mijn-darwin)
