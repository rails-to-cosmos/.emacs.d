(use-package nim-mode
  :hook ((nim-mode . smartparens-strict-mode))
  :config (progn
            (add-hook 'nim-mode-hook #'lsp-deferred))
  :ensure t
  :ensure flycheck-nim
  :ensure smartparens)

(provide 'init-nim)
