(require 'flycheck)
(require 'flycheck-indicator)

(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 5
      flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-indicator-mode)

;; (defun my/flycheck-direnv-setup ()
;;   (when (and (bound-and-true-p flycheck-mode) (executable-find "direnv"))
;;     (direnv-update-environment)))
;; (add-hook 'flycheck-mode-hook #'my/flycheck-direnv-setup)
