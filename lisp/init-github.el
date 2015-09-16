(use-package yagist
  :ensure t)
(use-package github-browse-file
  :ensure t)
(use-package bug-reference-github
  :ensure t)
;; (use-package git-gutter
;;   :config (global-git-gutter-mode t)
;;   :ensure t)

(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(use-package github-clone
    :ensure t)
(use-package magit-gh-pulls
  :ensure t)

(provide 'init-github)
