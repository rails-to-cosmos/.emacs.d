(use-package sh-script
  :hook ((sh-base-mode . eglot-ensure)
         (sh-base-mode . smartparens-strict-mode)
         (sh-base-mode . company-mode)))

(provide 'init-bash)
