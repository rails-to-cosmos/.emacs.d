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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package python
  :interpreter ("ipython" . python-mode)
  :init (progn
          (use-package jedi
            :config (progn
                      (add-hook 'python-mode-hook 'jedi:setup)
                      (setq jedi:complete-on-dot t)
                      (jedi:install-server))
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
            :ensure t))

  :config (progn
            (setq-default python-indent-offset 4)

            (defun python-highlight-breakpoints ()
              (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

            (defun python-add-breakpoint ()
              "Add a break point"
              (interactive)
              (insert "import ipdb; ipdb.set_trace()")
              (python-highlight-breakpoints))

            (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'python-mode-hook 'linum-mode))
  :ensure t)

(provide 'py)
;;; py.el ends here
