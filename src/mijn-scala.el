;; -*- lexical-binding: t; -*-
(up scala-mode
  :mode "\\.s\\(cala\\|bt\\|c\\)$"
  :hook ((scala-mode . flycheck-mode))
  :config (progn
            (setq scala-indent:step 2
                  scala-indent:align-parameters t
                  scala-indent:align-forms t)
            (add-to-list 'org-structure-template-alist '("scala" . "src amm"))
            (add-to-list 'ob-languages '(scala . t))
            (org-babel-do-load-languages 'org-babel-load-languages ob-languages))
  :ensure flycheck
  :ensure smartparens)

(up sbt-mode
  :commands sbt-start sbt-command
  :config (progn
            (substitute-key-definition
             'minibuffer-complete-word
             'self-insert-command
             minibuffer-local-completion-map)
            (setq sbt:program-options '("-Dsbt.supershell=false")))
  :ensure nil)

(up lsp-mode
  :hook ((scala-mode . lsp-deferred)
         (lsp-mode . lsp-lens-mode))
  :config (progn
            (setq lsp-prefer-flymake nil))
  :ensure nil)

(up lsp-metals
    :config (progn
              ;; (lsp-metals-toggle-show-implicit-arguments)
              ;; (lsp-metals-toggle-show-implicit-conversions)
              ;; (lsp-metals-toggle-show-inferred-type)
              ;; (lsp-metals-toggle-show-super-method-lenses)
              )
    :ensure t)

(up lsp-ui
    :ensure t)

;; (use-package company-lsp
;;     :ensure nil)

(up posframe
    :ensure nil)

(up dap-mode
    :ensure t)

(provide 'mijn-scala)
