;;; minimal-theme.el --- A minimal theme based on default colors.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License:
;; Use of this source code is governed by the 'Revised BSD License'
;; which can be found in the LICENSE file.

;;; Code:

(deftheme minimal-theme
  "A minimal theme based on xterm-256 color set [dark version]")

(custom-theme-set-faces
 'minimal-theme

 `(org-ellipsis                    ((t (:inherit hs-face))))

 ;; Mode-line
 `(mode-line ((t (:background "grey75" :box ,(list :line-width 4 :color "grey75")))))
 `(mode-line-inactive ((t (:background "grey90" :foreground "grey20" :box ,(list :line-width 4 :color "grey90"))))))

;; Set variables
(custom-theme-set-variables
 'minimal-theme)

(provide-theme 'minimal-theme)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; minimal-dark-theme.el ends here
