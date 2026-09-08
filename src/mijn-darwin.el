;; -*- lexical-binding: t; -*-

;; These are defined only in macOS Emacs builds; declare them so byte-compiling
;; on other platforms does not warn about free variables.
(defvar mac-command-modifier)
(defvar mac-option-modifier)
(defvar mac-redisplay-dont-reset-vscroll)
(defvar ns-use-native-fullscreen)
(defvar ns-pop-up-frames)
(defvar alert-default-style)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier  'meta
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
