(require 'direnv)

(cl-defun my-init-go ()
  (interactive)
  (let* ((project-root (direnv "go.mod"))
         (go-path (file-truename (concat project-root ".go")))
         (go-bin (f-join go-path "bin"))
         (path (->> (or (getenv "PATH") "")
                    (s-split ":")
                    (append (list go-bin) exec-path)
                    (seq-uniq))))
    (setenv "GOPATH" go-path)
    (setenv "PATH" (s-join ":" path))
    (setq-local exec-path path))

  (setq-local company-backends '(company-capf company-files)
              standard-indent 4
              go-ts-mode-indent-offset 4)

  (eglot-ensure)
  (indent-tabs-mode -1)
  (yas-minor-mode)
  (setq-local tab-width 4)
  (go-guru-hl-identifier-mode)
  (smartparens-strict-mode)
  (company-quickhelp-mode)
  (company-statistics-mode))

(use-package go-mode
  :hook (;; (go-mode . lsp-deferred)
         ;; (go-mode . set-go-mode-hooks)
         ;; (go-mode . company-mode)
         ;; (go-mode . my-nix-flake-activate-for-buffer)
         ;; (go-mode . eglot-ensure)

         (go-mode . my-init-go)

         ;; (before-save-hook . gofmt-before-save)
         )

  :ensure t
  :ensure lsp-mode
  :ensure smartparens
  :ensure go-guru
  :ensure company
  :ensure yasnippet)

(provide 'init-go)
