;;; init-c.el --- my c/cpp workflow
;;; Commentary:
;;; Code:

(defun c-lazy-init () "Initialize cpp with my configuration." (interactive))

(use-package irony
  :ensure t)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun irony-replace-completion-at-point ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(provide 'init-c)
;;; init-c.el ends here
