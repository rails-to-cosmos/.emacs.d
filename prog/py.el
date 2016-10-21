;;; py.el --- my python environment
;;
;; Filename: py.el
;; Description: my python environment
;; Author: Dmitry Akatov
;; Created: <2016-10-20 Thu 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d/prog/py.el
;; Keywords: Emacs 24.5
;; Compatibility: emacs >= 24.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun init-python () "Initialize python with my configuration." (interactive))

(use-package python
  :interpreter ("ipython" . python-mode)
  :bind (:map python-mode-map
              ("C-c C-b" . python-add-breakpoint)
              ("C-c C-g" . jedi:goto-definition))
  :config (progn
            (setq-default python-indent-offset 4)))

(use-package elpy
  :config (progn
            (elpy-enable)
            (jedi:install-server)
            (setq jedi:complete-on-dot t))
  :ensure t)

(use-package pungi
  :ensure t)

(use-package cinspect
  :ensure t)

(use-package py-isort
  :ensure t)

(use-package py-yapf
  :ensure t)

(use-package virtualenv
  :config (progn
            (use-package virtualenvwrapper
              :config (progn
                        (venv-initialize-interactive-shells)
                        (venv-initialize-eshell))
              :ensure t))
  :ensure t)

(defun python-highlight-breakpoints ()
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (insert "import ipdb; ipdb.set_trace()")
  (python-highlight-breakpoints))

(add-hook 'python-mode-hook 'python-highlight-breakpoints)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'linum-mode)

(provide 'py)
;;; py.el ends here
