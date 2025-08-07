(require 'files)

(use-package mise
  :ensure t)

(use-package envrc
  :ensure t)

(cl-defun my-direnv ()
  (interactive)
  (let ((direnv-file-mode-map '(("mise.toml" mise-mode)
                                (".envrc" envrc-mode))))
    (cl-loop for (mode-file mode-hook) in direnv-file-mode-map
             when (locate-dominating-file default-directory mode-file)
             do (funcall mode-hook))))

(provide 'init-direnv)
