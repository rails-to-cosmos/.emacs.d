(require 'eglot)
(require 'cc-mode)
(require 'company)
(require 'company-quickhelp)

(use-package eglot-java
  :ensure t)

(add-hook 'java-mode-hook #'eglot-ensure)
(add-hook 'java-mode-hook #'company-mode)
(add-hook 'java-mode-hook #'company-quickhelp-mode)

(provide 'init-java)
