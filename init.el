;;; init.el --- my emacs configuration
;;; Commentary:
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(dolist (package-archive
         '(("melpa" . "http://melpa.milkbox.net/packages/")
           ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
           ("org" . "https://orgmode.org/elpa/")
           ("marmalade" . "http://marmalade-repo.org/packages/")
           ("elpy" . "https://jorgenschaefer.github.io/packages/")))
  (add-to-list 'package-archives package-archive))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package org
  :ensure t)

(org-babel-load-file
 (expand-file-name "emacs.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
(put 'set-goal-column 'disabled nil)
