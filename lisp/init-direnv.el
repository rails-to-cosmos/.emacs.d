(use-package mise
  :ensure t)

(use-package envrc
  :ensure t)

(cl-defun my-direnv ()
  (interactive)
  (or (-some->> (locate-dominating-file (buffer-file-name) "mise.toml")
        (mise-mode))
      (-some->> (locate-dominating-file (buffer-file-name) ".envrc")
        (envrc-mode))))

(provide 'init-direnv)
