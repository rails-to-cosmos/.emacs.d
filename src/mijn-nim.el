(use-package nim-mode
  :config (progn
            (add-hook 'nim-mode-hook #'lsp-deferred))
  :ensure nil
  :ensure flycheck-nim
  :ensure smartparens)

(provide 'mijn-nim)
