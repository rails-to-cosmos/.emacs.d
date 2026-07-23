(use-package zig-mode
  :hook ((zig-mode . lsp-deferred)
         (zig-mode . abbrev-mode)
         (zig-mode . flycheck-mode)
         (zig-mode . company-quickhelp-mode))

  :ensure lsp-mode
  :ensure nil)

(provide 'mijn-zig)
