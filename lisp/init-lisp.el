(require 'rainbow-delimiters)
(require 'paredit)
(require 'expal)

;; (use-package elsa
;;   :ensure elsa-flycheck
;;   :ensure t)

(use-package eask
  :config (progn
            (require 'eask-mode))
  :ensure t
  :ensure eask-mode)

(use-package buttercup
  :ensure t)

;; (require 'eval-sexp-fu)

(add-hook 'lisp-data-mode-hook 'smartparens-strict-mode)

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'smartparens-strict-mode)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'emacs-lisp-eval-expal)

;; (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-completion-setup)
;; (cl-defun emacs-lisp-completion-setup ()
;;   (setq-local company-backends '(company-elisp company-files company-yasnippet)))

(cl-defun emacs-lisp-eval-expal ()
  (interactive)
  (save-excursion
    (let ((bounds (expal:bounds)))
      (expal:highlight)
      (goto-char (cdr bounds))
      (eval-last-sexp nil)
      (set-mark (car bounds))
      (goto-char (cdr bounds))
      (indent-for-tab-command)
      (deactivate-mark))))

;; (use-package elsa
;;   :ensure flycheck-elsa
;;   :ensure t)

(provide 'init-lisp)
