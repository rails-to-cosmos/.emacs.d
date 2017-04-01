;;; init-web.el --- provide web-oriented packages
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode ("\\.html\\'" "\\.ejs\\'" "\\.htm\\'" "\\.jsx\\'")
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4)
  :ensure web-beautify
  :ensure t)

(use-package emmet-mode
  :mode ("\\.html\\'" "\\.htm\\'")
  :config (add-hook 'web-mode-hook 'emmet-mode)
  :ensure t)

(use-package json-mode
  :commands (json-mode json-reformat-region)
  :ensure t)

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :commands js2-mode
  :ensure t)

(use-package ac-js2
  :commands ac-js2
  :ensure t)

(use-package js-comint
  :commands js-comint
  :ensure t)

(provide 'init-web)
;;; init-web.el ends here
