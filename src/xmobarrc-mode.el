;;; xmobarrc-mode.el --- Major mode for xmobar configuration files -*- lexical-binding: t; -*-

(defvar xmobarrc-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    st)
  "Syntax table for `xmobarrc-mode'.")

(defvar xmobarrc-font-lock-keywords
  (let ((config-keys '("font" "additionalFonts" "bgColor" "fgColor" "position"
                        "lowerOnStart" "hideOnStart" "allDesktops" "persistent"
                        "commands" "sepChar" "alignSep" "template"
                        "border" "borderColor" "borderWidth"
                        "alpha" "overrideRedirect" "pickBroadest"
                        "iconRoot" "iconOffset" "textOffset" "textOffsets"
                        "wmClass" "wmName"))
        (commands '("Run" "Cpu" "Com" "Memory" "DiskU" "BatteryP"
                    "Wireless" "Alsa" "Kbd" "Date" "StdinReader"
                    "Network" "CoreTemp" "Weather" "Mail"
                    "TopProc" "TopMem" "Swap" "MultiCpu"
                    "DynNetwork" "Uptime" "CatInt" "UnsafeStdinReader"
                    "XPropertyLog" "NamedXPropertyLog" "Brightness"
                    "Volume" "Mpris2" "MultiCoreTemp"))
        (constructors '("Config" "TopH" "TopHM" "BottomH" "BottomHM"
                        "TopW" "BottomW" "TopP" "BottomP"
                        "Static" "OnScreen" "Top" "Bottom"))
        (constants '("True" "False")))
    `((,(regexp-opt config-keys 'symbols) . font-lock-variable-name-face)
      (,(regexp-opt commands 'symbols) . font-lock-function-name-face)
      (,(regexp-opt constructors 'symbols) . font-lock-type-face)
      (,(regexp-opt constants 'symbols) . font-lock-constant-face)
      ("%[a-zA-Z:_][a-zA-Z0-9:_]*%" . font-lock-preprocessor-face)
      ("<fn=[0-9]+>" . font-lock-builtin-face)
      ("</fn>" . font-lock-builtin-face)
      ("<fc=[^>]*>" . font-lock-builtin-face)
      ("</fc>" . font-lock-builtin-face)
      ("#[0-9a-fA-F]\\{6\\}" . font-lock-string-face)))
  "Font lock keywords for `xmobarrc-mode'.")

;;;###autoload
(define-derived-mode xmobarrc-mode prog-mode "Xmobarrc"
  "Major mode for editing xmobar configuration files."
  :syntax-table xmobarrc-mode-syntax-table
  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local font-lock-defaults '(xmobarrc-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("xmobarrc\\'" . xmobarrc-mode))

(provide 'xmobarrc-mode)
;;; xmobarrc-mode.el ends here
