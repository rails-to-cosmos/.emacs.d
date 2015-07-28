(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  ;; (newline-and-indent)
  (insert "import pdb; pdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))

(add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

(define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)
(hl-line-mode t)

;; python-mode
;; ============
;; pre-requisites on ubuntu
;; sudo pip install --upgrade pip
;; sudo pip install jedi json-rpc --upgrade

;; Deprecate these and try on a fresh machine new.
;; sudo apt-get install -y python-dev python-setuptools python-pip python-virtualenv virtualenvwrapper
;; sudo apt-get install -y python-flake8 pylint pep8 python-autopep8 python-jedi python-six
;; Experimenting with python3, but not gotten it working yet
;; (setq python-python-command "/home/sid/.virtualenvs/emacs/bin/python")
;; (setq python-shell-interpreter "/usr/bin/python3")
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode nil)
;;             (setq tab-width 4)
;;             (setq python-indent-offset 4)))

;; anaconda
;; (use-package anaconda-mode
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   (add-hook 'python-mode-hook 'eldoc-mode))
;; (use-package company-anaconda
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-anaconda))

;; virtualenv
;; (use-package virtualenvwrapper
;;   :ensure t
;;   :config
;;   (venv-initialize-interactive-shells) ;; if you want interactive shell support
;;   (venv-initialize-eshell) ;; if you want eshell support
;;   (setq venv-location "~/.virtualenvs/"))
;; (setq python-shell-virtualenv-path "~/.virtualenvs/default")

;; pep8
(use-package py-autopep8
 :ensure t)
(setq py-autopep8-options '("--ignore=E309,"))

;; (defun python-interactive ()
;;   "Enter the interactive Python environment"
;;   (interactive)
;;   (progn
;;     (insert "!import code; code.interact(local=vars())")
;;     (move-end-of-line 1)
;;     (comint-send-input)))

;; (global-set-key (kbd "C-c i") 'python-interactive)

(provide 'init-python-mode)
