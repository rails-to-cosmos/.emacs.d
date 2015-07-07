(deftheme spacegray-eighties
  "Created 2014-09-06.")

(custom-theme-set-faces
 'spacegray-eighties

 ;; Basic
 '(button ((t (:inherit (link)))))

 (set-cursor-color "#c5c8c6")

 '(default ((t (:foreground "#c5c8c6"
                            :background "#2d2d2d"
                            :weight normal
                            :slant normal
                            :underline nil
                            :overline nil
                            :strike-through nil
                            :box nil
                            :inverse-video nil
                            :stipple nil
                            :inherit nil))))

 '(escape-glyph ((t (:foreground "#FF8000"))))

 '(fixed-pitch ((t (:family "Monospace"))))

 '(header-line ((t (:foreground "grey90" :background "grey20"))))

 '(highlight ((t (:background "#393939"))))

 '(lazy-highlight ((((class color)
                     (min-colors 88)
                     (background light))
                    (:background "paleturquoise"))

                   (((class color)
                     (min-colors 88)
                     (background dark))
                    (:background "paleturquoise4"))

                   (((class color)
                     (min-colors 16))
                    (:background "turquoise3"))

                   (((class color)
                     (min-colors 8))
                    (:background "turquoise3"))

                   (t (:underline (:color foreground-color :style line)))))

 '(link ((t (:inherit font-lock-keyword-face :underline t))))

 '(link-visited ((default (:inherit (link)))

                 (((class color)
                   (background light))
                  (:foreground "magenta4"))

                 (((class color)
                   (background dark))
                  (:foreground "violet"))))

 '(match ((((class color)
            (min-colors 88)
            (background light))
           (:background "yellow1"))
          (((class color)
            (min-colors 88)
            (background dark))
           (:background "RoyalBlue3"))

          (((class color)
            (min-colors 8)
            (background light))
           (:foreground "#2d2d2d" :background "yellow"))

          (((class color)
            (min-colors 8)
            (background dark))

           (:foreground "white" :background "blue"))
          (((type tty)
            (class mono))
           (:inverse-video t))
          (t (:background "gray"))))

 '(minibuffer-prompt ((t (:foreground "#FF8000"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(region ((t (:background "#515151"))))
 '(secondary-selection ((t (:background "#262626"))))
 '(shadow ((t (:foreground "#7c7c7c"))))
 '(tooltip ((t (:inherit variable-pitch :background "#fff" :foreground "#333"))))
 '(trailing-whitespace ((t (:background "#562d56" :foreground "#FD5FF1"))))
 '(variable-pitch ((t (:family "Sans Serif"))))

 ;; dired-mode
 '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
 '(dired-flagged ((t (:inherit (diff-hl-delete)))))
 '(dired-symlink ((t (:foreground "#FD5FF1"))))

 ;; org-mode
 '(org-agenda-date ((t (:foreground "LightSkyBlue"))))
 '(org-agenda-date-weekend ((t (:bold t :foreground "LightSkyBlue" :weight bold))))
 '(org-agenda-restriction-lock ((t (:background "skyblue4"))))
 '(org-agenda-structure ((t (:foreground "LightSkyBlue"))))
 '(org-archived ((t (:foreground "grey70"))))
 '(org-code ((t (:foreground "grey70"))))
 '(org-column ((t (:background "grey30" :strike-through nil :underline nil :slant normal :weight normal :height 81 :family "unknown-DejaVu Sans Mono"))))
 '(org-column-title ((t (:bold t :background "grey30" :underline t :weight bold))))
 '(org-date ((t (:foreground "Cyan" :underline t))))
 '(org-done ((t (:bold t :foreground "PaleGreen" :weight bold))))
 '(org-drawer ((t (:foreground "LightSkyBlue"))))
 '(org-ellipsis ((t (:foreground "LightGoldenrod" :underline t))))
 '(org-formula ((t (:foreground "chocolate1"))))
 '(org-headline-done ((t (:foreground "LightSalmon"))))
 '(org-hide ((t (:foreground "black"))))
 '(org-latex-and-export-specials ((t (:foreground "burlywood"))))
 '(org-level-1 ((t (:foreground "LightSkyBlue"))))
 '(org-level-2 ((t (:foreground "LightGoldenrod"))))
 '(org-level-3 ((t (:foreground "Cyan1"))))
 '(org-level-4 ((t (:foreground "chocolate1"))))
 '(org-level-5 ((t (:foreground "PaleGreen"))))
 '(org-level-6 ((t (:foreground "Aquamarine"))))
 '(org-level-7 ((t (:foreground "LightSteelBlue"))))
 '(org-level-8 ((t (:foreground "LightSalmon"))))
 '(org-link ((t (:foreground "Cyan" :underline t))))
 '(org-property-value ((t (nil))))
 '(org-scheduled-previously ((t (:foreground "chocolate1"))))
 '(org-scheduled-today ((t (:foreground "PaleGreen"))))
 '(org-sexp-date ((t (:foreground "Cyan"))))
 '(org-special-keyword ((t (:foreground "LightSalmon"))))
 '(org-table ((t (:foreground "LightSkyBlue"))))
 '(org-tag ((t (:bold t :weight bold))))
 '(org-target ((t (:underline t))))
 '(org-time-grid ((t (:foreground "LightGoldenrod"))))
 '(org-todo ((t (:bold t :foreground "Pink" :weight bold))))
 '(org-upcoming-deadline ((t (:foreground "chocolate1"))))
 '(org-verbatim ((t (:foreground "grey70" :underline t))))
 '(org-warning ((t (:bold t :weight bold :foreground "Pink"))))
 '(outline-1 ((t (:foreground "LightSkyBlue"))))
 '(outline-2 ((t (:foreground "LightGoldenrod"))))
 '(outline-3 ((t (:foreground "Cyan1"))))
 '(outline-4 ((t (:foreground "chocolate1"))))
 '(outline-5 ((t (:foreground "PaleGreen"))))
 '(outline-6 ((t (:foreground "Aquamarine"))))
 '(outline-7 ((t (:foreground "LightSteelBlue"))))
 '(outline-8 ((t (:foreground "LightSalmon"))))

 ;; mode-line
 '(mode-line ((t (:background "#2d2d2d" :foreground "#96CBFE"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88))
                         (:box (:line-width -1 :color "#2d2d2d" :style nil)))
                        (t (:inherit (highlight)))))
 '(mode-line-inactive ((default (:inherit (mode-line)))
                       (((class color)
                         (min-colors 88)
                         (background light))
                        (:background "#2d2d2d" :foreground "grey20" :box (:line-width -1 :color "#2d2d2d" :style nil) :weight light))
                       (((class color)
                         (min-colors 88)
                         (background dark))
                        (:background "#2d2d2d" :foreground "grey80" :box (:line-width -1 :color "#2d2d2d" :style nil) :weight light))))

 ;; Font-lock
 '(font-lock-builtin-face ((t (:foreground "#DAD085"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#7C7C7C"))))
 '(font-lock-constant-face ((t (:foreground "#99CC99"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#FFD2A7"))))
 '(font-lock-keyword-face ((t (:foreground "#96CBFE"))))
 '(font-lock-preprocessor-face ((t (:foreground "#8996A8"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-string-face))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#C6A24F"))))
 '(font-lock-string-face ((t (:foreground "#8AE234"))))
 '(font-lock-type-face ((t (:foreground "#CFCB90"))))
 '(font-lock-variable-name-face ((t (:inherit (default)))))
 '(font-lock-warning-face ((t (:foreground "#ff982d" :weight bold))))

 '(secondary-selection ((((class color)
                          (min-colors 88)
                          (background light))
                         (:background "#e1b157")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "#e1b157")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "#2d2d2d" :background "#529f9d")) (t (:inverse-video t))))
 '(trailing-whitespace ((t (:background "#2d2d2d"))))

 '(fringe ((((type ns)) (:foreground "#d3d0c8" :background "#2d2d2d")) (((class color) (background light)) (:background "#2d2d2d")) (((class color) (background dark)) (:background "#2d2d2d")) (t (:background "#2d2d2d"))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "#2d2d2d" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "#2d2d2d"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "#2d2d2d" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark))
                                                                                                                       (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "#529f9d" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "#529f9d" :background "magenta4")) (t (:inverse-video t))))

 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))

 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))

 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "#2d2d2d" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))

 '(next-error ((t (:inherit (region)))))

 '(query-replace ((t (:inherit (isearch)))))

  (custom-set-faces
   '(default ((t (
                  :inherit nil
                           :stipple nil
                           :background "#2d2d2d"
                           :foreground "#d3d0c8"
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
                           :family "Inconsolata"))))))

(provide-theme 'spacegray-eighties)
