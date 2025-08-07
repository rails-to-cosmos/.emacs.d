(require 'files)

(use-package mise
  :ensure t)

(use-package envrc
  :ensure t)

(cl-defun my-direnv ()
  (interactive)
  (or (-some->> (locate-dominating-file default-directory "mise.toml")
        (mise-mode))
      (-some->> (locate-dominating-file default-directory ".envrc")
        (envrc-mode))))

(provide 'init-direnv)
