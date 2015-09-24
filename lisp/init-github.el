(use-package yagist
  :ensure t)
(use-package github-browse-file
  :ensure t)
(use-package bug-reference-github
  :ensure t)
(use-package git-gutter+
  :config
  (add-hook 'python-mode-hook 'git-gutter+-mode)
  :ensure t)

(use-package git-gutter-fringe+
  :config
  (git-gutter-fr+-minimal))

(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(use-package github-clone
  :ensure t)
(use-package magit-gh-pulls
  :ensure t)

(provide 'init-github)
