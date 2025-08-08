(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control
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

(provide 'init-darwin)
