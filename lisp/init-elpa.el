(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(setq package-enable-at-startup nil)

(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package fullframe)
(fullframe list-packages quit-window)


(provide 'init-elpa)
