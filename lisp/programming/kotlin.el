(require 'lsp-mode)
(require 'lsp-kotlin)

(setq lsp-clients-kotlin-server-executable
      (f-join user-emacs-directory "stuff" "kotlin-language-server.nixos" "kotlin-language-server"))

(cl-defun my-kotlin-completion-setup ()
  (setq-local company-backends '(company-elisp company-files)))

(define-key citre-mode-map (kbd "M-.") #'citre-jump)

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-completion-setup)

(add-hook 'kotlin-mode-hook #'lsp-mode)
(add-hook 'kotlin-mode-hook #'citre-mode)
