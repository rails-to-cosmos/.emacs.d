;; -*- lexical-binding: t; -*-
(use-package flycheck-nim
  :ensure t)

(use-package nim-mode
  :config (progn
            (add-hook 'nim-mode-hook #'lsp-deferred))
  :ensure t)

(provide 'mijn-nim)
