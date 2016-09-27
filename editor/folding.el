(use-package origami
  :config (progn
            (add-hook 'prog-mode-hook 'origami-mode)
            (add-hook 'emacs-lisp-mode-hook 'origami-mode))
  :ensure t)

(provide 'folding)
