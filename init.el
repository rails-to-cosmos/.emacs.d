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

(setq ;; use-package-always-defer t
 use-package-always-ensure t
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

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
