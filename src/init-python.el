(require 'files)

(require 'direnv)

(require 'init-org)
(require 'init-completion)

(cl-defun my-python-mode-hook ()
  (require 'pyvenv)
  (require 'eglot)
  (require 'company)
  (require 'flycheck)
  (require 'yasnippet)
  (require 'subword)
  (require 'abbrev)
  (require 'company-quickhelp)
  (require 'smartparens)

  ;; (require 'flymake)

  (direnv)
  (eglot-ensure)
  (company-mode)
  (flycheck-mode)
  (yas-minor-mode)
  (subword-mode)
  (smartparens-strict-mode)
  (python-highlight-breakpoints)
  (company-quickhelp-mode)
  (abbrev-mode)

  ;; (flymake-mode)

  (setq-local company-backends '(company-files
                                 company-dabbrev-code
                                 (company-capf :with company-yasnippet)))

  (setq-local flycheck-checkers '(python-flake8
                                  python-pylint
                                  python-pycompile
                                  python-pyright
                                  python-mypy
                                  python-pycodestyle)))

(use-package python-mode
  :defer
  :init (ob-add-language 'python (cons "python" "src python"))
  :hook ((python-mode . my-python-mode-hook)
         ;; (inferior-python-mode . my-python-mode-hook)
         )

  :bind (:map python-mode-map
              ("C-c C-c" . my-python-paragraph-eval)
              ("C-c C-k" . my-python-kill-comments)
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))

  :ensure mise
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
  (highlight-lines-matching-regexp "pdb.set_trace()")
  (highlight-lines-matching-regexp "breakpoint()")
  (highlight-phrase "TODO")
  (highlight-regexp "FIXME"))

(define-abbrev-table 'python-mode-abbrev-table
  '(("pdb" "breakpoint()" nil 0)
    ("cpdb" "context.pdb.set_trace()" nil 0)))

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
