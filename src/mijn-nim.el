;; -*- lexical-binding: t; -*-
(up flycheck-nim
  :ensure t)

(up nim-mode
  :config (progn
            (add-hook 'nim-mode-hook #'lsp-deferred))
  :ensure t)

(provide 'mijn-nim)
