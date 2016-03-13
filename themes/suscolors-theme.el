;;; suscolors-theme.el --- Colorful theme, inspired by Gruvbox.

;; Copyright (c) 2016 Tomas Vojtisek

;; Author: Tomas Vojtisek <suspiciouswombat@gmail.com>
;; URL: https://github.com/TheSuspiciousWombat/SusColors-emacs
;; Package-Version: 20160311.458


;;; Commentary:

;;; Code:
(deftheme suscolors
  "Created 2016-01-25.")
(let ((bg "#000000")
      (fg "#ffffff")
      (black "#000000")
      (yellow "#FFDB45")
      (orange "#F9BA32")
      (comment "#8593AE")
      (grey "#3a3f4b")
      (grey2 "#21252b")
      (grey3 "#282c34")
      (green "#B3DE81")
      (blue "#D0E1F9" )
      (pink "#FF87BA")
      (red "#F34A4A")
      (purple "#4CB5F5"))

  (custom-theme-set-faces
   'suscolors
   `(default ((t (:foreground ,fg :background ,bg))))
   `(fringe ((t (:background ,bg))))
   `(font-lock-string-face ((t (:foreground ,"#E38B75"))))
   `(font-lock-builtin-face ((t (:foreground ,purple))))
   `(region ((t (:background ,grey))))
   `(font-lock-variable-name-face ((t (:foreground ,yellow))))
   `(font-lock-keyword-face ((t (:foreground ,blue))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,grey))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:foreground ,orange))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(button ((t (:foreground ,blue :underline t))))
   `(link ((t (:foreground ,blue))))
   `(menu ((t (:foreground ,fg :background ,grey2))))

   `(mode-line ((t (:background ,grey2
                    :foreground ,fg
                    :box ,(list
                           :line-width 4
                           :color grey2)))))
   `(mode-line-inactive ((t (:background ,grey3
                             :foreground "#FFF"
                             :box ,(list
                                    :line-width 4
                                    :color grey3)))))
   `(mode-line-buffer-id ((t (:foreground ,fg))))


   `(font-lock-warning-face ((t (:foreground ,red))))
   `(compilation-warning ((t (:foreground ,red))))
   `(highlight ((t (:background ,grey2 :foreground ,blue))))

   `(linum ((t (:foreground ,grey))))

   `(widget-field ((t (:foreground ,fg :background ,grey3))))

   ;; Highlight quoted mode-line
   `(highlight-quoted-symbol ((t (:foreground ,pink))))

   ;; hl-line and hlinum-activate
   `(linum-highlight-face ((t (:foreground ,grey :background ,grey2 :weight bold))))
   `(hl-line ((t (:background ,grey2))))

   ;; magit
   ;;`(magit-diff-added-highlight ((t (:background ,"#35B82C" :foreground ,fg))))
   ;;`(magit-diff-removed-highlight ((t (:background "yellow"))))
   ;;`(magit-diff-context-highlight ((t (:background ,bg))))

   ;; Org
   `(org-todo ((t (:foreground ,red))))
   `(org-done ((t (:foreground ,green))))
   `(org-date ((t (:foreground ,yellow))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,pink))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,orange))))
   ;; Company-mode
   `(company-tooltip ((t (:foreground ,fg :background ,grey2))))
   `(company-tooltip-selection ((t (:foreground ,fg :background ,grey3))))
   `(company-scrollbar-fg ((t (:background ,grey2))))
   `(company-scrollbar-bg ((t (:background ,grey3))))
   `(company-tooltip-common ((t (:foreground ,orange))))
   `(company-preview ((t (:background ,grey3))))
   `(company-preview-common ((t (:background ,grey3 :foreground ,red))))
   `(company-mouse ((t (:background ,grey2))))

   ;; Flycheck
   `(flycheck-warning ((t (:foreground ,red :underline t))))

   ;; js2-mode
   `(js2-function-param ((t (:foreground ,orange))))

   ;; erc
   `(erc-timestamp-face ((t (:foreground ,red))))
   `(erc-prompt-face ((t (:foreground ,green))))
   `(erc-nick-default-face ((t (:foreground ,blue))))
   `(erc-notice-face ((t (:foreground ,pink))))
   `(erc-button ((t (:foreground ,blue))))
   `(erc-current-nick-face ((t (:foreground ,red))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,red))))
   `(eshell-ls-executable ((t (:foreground ,green))))
   `(eshell-ls-directory ((t (:foreground ,blue))))

   ;; ido
   `(minibuffer-prompt ((t (:foreground ,comment))))
   `(ido-first-match ((t (:foreground ,blue))))
   `(ido-only-match ((t (:foreground ,blue))))
   `(ido-subdir ((t (:foreground ,blue))))
   `(ido-vertical-match-face ((t (:foreground ,purple))))

   ;; vertical-border
   `(vertical-border ((t (:foreground "#282a2e"))))
   `()
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'suscolors)
;;; suscolors-theme.el ends here
