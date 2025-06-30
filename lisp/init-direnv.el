(use-package mise
  :ensure t)

(use-package envrc
  :config (progn
            (envrc-global-mode))
  :ensure t)

(provide 'init-direnv)
