(require 'prog-mode)
(require 'direnv)

(add-hook 'prog-mode-hook #'direnv)

(provide 'init-prog)
