(use-package zig-mode
  :hook ((zig-mode . lsp-deferred)
         (zig-mode . abbrev-mode)
         (zig-mode . flycheck-mode)
         (zig-mode . smartparens-strict-mode)
         (zig-mode . company-mode)
         (zig-mode . company-quickhelp-mode)
         (zig-mode . subword-mode))

  :ensure lsp-mode
  :ensure t)

(provide 'init-zig)
