(ob-add-language 'emacs-lisp (cons "el" "src emacs-lisp"))
(setq lisp-indent-function 'common-lisp-indent-function)

;; (ql:quickload "quicklisp-slime-helper")
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(require 'ts)

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(setq byte-compile-warnings '(not cl-functions))

(cl-defun emacs-lisp-completion-setup ()
  (setq-local company-backends '(company-elisp company-files)))

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

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-completion-setup)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'emacs-lisp-eval-expal)
