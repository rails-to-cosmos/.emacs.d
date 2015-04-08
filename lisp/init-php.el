(use-package php-mode
  :bind (("C-c C-e" . php-eval-buffer)
         ("C-." . imenu-anywhere))
  :config (defun my-php-mode-stuff ()
             (subword-mode 1)
             (hs-minor-mode 1)
             )
  (add-hook 'php-mode-hook 'my-php-mode-stuff))

(add-auto-mode 'web-mode "\\.html\\'")

(use-package smarty-mode)

(provide 'init-php)
