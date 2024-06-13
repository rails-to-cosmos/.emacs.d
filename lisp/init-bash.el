(use-package sh-script
  :hook ((sh-base-mode . eglot-ensure)
         (sh-base-mode . smartparens-strict-mode)))

(provide 'init-bash)
