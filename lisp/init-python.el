(require 'files)

(require 'init-org)
(require 'init-completion)

(use-package python-mode
  :defer

  :init (progn
          (ob-add-language 'python (cons "python" "src python")))

  :config (progn
            (setq-default flycheck-checkers '(python-flake8
                                              python-pylint
                                              python-pycompile
                                              python-pyright
                                              python-mypy
                                              python-pycodestyle)))

  :hook ((python-mode . abbrev-mode)
         (python-mode . company-quickhelp-mode)
         (python-mode . python-highlight-breakpoints)
         (python-mode . smartparens-strict-mode)
         (python-mode . subword-mode)
         (python-mode . yas-minor-mode)
         (python-mode . (lambda ()
                          (setq-local company-backends '(company-files (company-capf :with company-yasnippet) company-dabbrev-code))
                          (eglot-ensure)
                          (company-mode)
                          (flycheck-mode)
                          (flymake-mode)))

         (inferior-python-mode . smartparens-strict-mode))

  :bind (:map python-mode-map
              ("C-c C-c" . my-python-paragraph-eval)
              ("C-c C-k" . my-python-kill-comments)
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))

  :ensure pyvenv-auto
  :ensure yasnippet
  :ensure yasnippet-capf
  :ensure jinja2-mode
  :ensure poetry
  :ensure eglot
  :ensure company
  :ensure pyimpsort
  :ensure py-autopep8
  :ensure flycheck
  :ensure flycheck-mypy
  :ensure flymake-ruff
  :ensure ruff-format
  :ensure lsp-pyright)

(flycheck-define-checker python-pycodestyle
  "A Python syntax and style checker using pycodestyle (former pep8)."

  :command ("pycodestyle" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes python-mode)

(defun python-highlight-breakpoints ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "set_trace()")
  (highlight-phrase "TODO")
  (highlight-regexp "FIXME"))

;; (add-to-list 'ob-languages '(python . t))
;; (org-babel-do-load-languages 'org-babel-load-languages ob-languages)

(defun my-get-thing-at-point (thing)
  (condition-case nil
      (substring-no-properties (thing-at-point thing))
    (error "")))

(defun my-python-send-buffer ()
  (interactive)
  (run-python)
  (python-shell-send-buffer)
  (save-window-excursion
    (switch-to-buffer-other-window (python-shell-get-buffer))))

(define-key python-mode-map (kbd "C-c C-l") #'my-python-send-buffer)
(define-key python-mode-map (kbd "M-/") #'hippie-expand)

(provide 'init-python)
