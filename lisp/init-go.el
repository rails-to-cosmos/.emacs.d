(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         ;; (go-mode . set-go-mode-hooks)
         ;; (go-mode . company-mode)
         (go-mode . (lambda ()
                      (setq-default company-backends '(company-capf company-files)
                                    ;; standard-indent 4
                                    ;; go-ts-mode-indent-offset 4

                                    )
                      (indent-tabs-mode -1)
                      (yas-minor-mode)
                      (setq-local tab-width 4)
                      (go-guru-hl-identifier-mode)
                      (smartparens-strict-mode)
                      (company-quickhelp-mode)
                      (company-statistics-mode)))
         (before-save-hook . gofmt-before-save))

  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark))

  :custom ((gofmt-command "goimports"))

  :ensure t
  :ensure lsp-mode
  :ensure smartparens
  :ensure go-guru
  :ensure company
  :ensure yasnippet)

(provide 'init-go)
