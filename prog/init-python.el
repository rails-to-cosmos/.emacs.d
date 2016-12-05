;;; init-python.el --- my python environment
;;
;; Filename: init-python.el
;; Description: my python environment
;; Author: Dmitry Akatov
;; Created: <2016-10-20 Thu 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d/prog/python.el
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

(defun python-add-breakpoint ()
  "Add a break point."
  (interactive)
  (insert "import pdb; pdb.set_trace()")
  (python-highlight-breakpoints))

(defun python-highlight-breakpoints ()
  "Highlight python breakpoints."
  (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))

(use-package elpy
  :interpreter ("ipython" . python-mode)
  :config (progn
            (elpy-enable)
            (jedi:install-server)
            (setq-default jedi:complete-on-dot t
                          python-indent-offset 4))
  :ensure t)

(use-package pungi
  :ensure t)

(use-package cinspect
  :ensure t)

(use-package py-isort
  :ensure t)

(use-package py-yapf
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package virtualenv
  :config (progn
            (use-package virtualenvwrapper
              :config (progn
                        (venv-initialize-interactive-shells)
                        (venv-initialize-eshell))
              :ensure t))
  :ensure t)

(provide 'init-python)
;;; init-python.el ends here
