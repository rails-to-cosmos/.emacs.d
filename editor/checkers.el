(use-package flycheck
  :config (progn
            (add-hook 'after-init-hook 'global-flycheck-mode)
            (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
                  flycheck-idle-change-delay 0.8
                  flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))
  :ensure t)

(provide 'checkers)
