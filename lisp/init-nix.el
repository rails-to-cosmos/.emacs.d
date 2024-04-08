(use-package nix-mode
  :hook ((nix-mode . smartparens-strict-mode))
  :ensure t)

(provide 'init-nix)
