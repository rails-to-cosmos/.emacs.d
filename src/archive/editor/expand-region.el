(require 'expand-region)

(setq shift-select-mode nil)

;; (defun my-expand-region ()
;;   (interactive)
;;   (when (thing-at-point 'word)
;;     (backward-word 1))
;;   (call-interactively #'er/expand-region))

;; from http://endlessparentheses.com/where-do-you-bind-expand-region.html
(global-set-key (kbd "M-2") #'er/expand-region)
