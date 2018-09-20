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

(add-to-list 'safe-local-variable-values '(eval ov-highlight-load))
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))

(provide 'init)
;;; init.el ends here

(fset 'jesshop-dup-inc-size
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([3 112 16 134217830 4 14 134217826 134217830 134217830 escape 120 return 134217830 escape 120 return escape 120 return 1 tab] 0 "%d")) arg)))

(fset 'jessica/check-color
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 60 111 102 102 101 114 32 105 100 return 19 112 105 99 116 117 114 101 return 19 104 116 116 112 return escape 120 98 114 111 119 115 101 45 117 114 108 return return 19 112 97 114 97 109 32 110 97 109 101 19 19 19 19 6 6 6 6 6 6 6 6 134217778] 0 "%d")) arg)))

(fset 'jessica/next-offer
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 82 85 34 62 53 52 return] 0 "%d")) arg)))

(defun jessica/check-offer-batch ()
  (interactive)
  (jessica/check-color)
  (search-forward "Серый")
  (backward-char)
  (jessica/color-to-hello)
  (replace-string "Hello" (read-string "Color: " "Серый")))

(fset 'jessica/color-to-hello
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217778 67108926 67108926 67108926 67108926 67108926 67108926 backspace 72 101 108 108 111 return] 0 "%d")) arg)))
