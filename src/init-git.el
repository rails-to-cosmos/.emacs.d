(use-package magit
  :config (progn
            (define-key global-map (kbd "C-x g") #'magit-status))
  :ensure t)

(provide 'init-git)
