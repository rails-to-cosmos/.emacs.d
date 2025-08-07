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
             when (-some->> (locate-dominating-file default-directory mode-file)
                    (funcall mode-hook))
             return (message "%s evaluated with my-direnv" mode-hook))))

(provide 'init-direnv)
