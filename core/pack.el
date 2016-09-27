(require 'package)

(defvar dist-packages-dir)
(setq dist-packages-dir (concat user-emacs-directory "packages/"))

(defvar package-user-dir)
(setq package-user-dir (concat dist-packages-dir "elpa/"))

(make-directory dist-packages-dir t)
(make-directory package-user-dir t)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :init (progn
          (use-package diminish)
          (use-package bind-key))
  :config (progn
            (setq use-package-verbose t)))

(provide 'pack)
