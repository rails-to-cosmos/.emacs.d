(setenv "LSP_USE_PLISTS" "true")
(setq read-process-output-max (* 3 1024 1024))

;; (require 'lsp)
;; (require 'lsp-haskell)
;; ;; Hooks so haskell and literate haskell major modes trigger LSP setup
;; (add-hook 'haskell-mode-hook #'lsp-deferred)
;; (add-hook 'haskell-literate-mode-hook #'lsp-deferred)

(use-package haskell-mode
  :init (progn
          (require 'haskell)
          (require 'lsp)
          (require 'flyspell)
          (require 'lsp-ui)
          (require 'which-key)

          (defun my-haskell-mode-hook ()
            (add-hook 'after-save-hook 'haskell-mode-generate-tags nil t)))
  :defines interactive-haskell-mode-map
  :config (progn
            (cl-pushnew "[/\\\\]\\.devenv\\'" lsp-file-watch-ignored-directories)
            (cl-pushnew "[/\\\\]\\.direnv\\'" lsp-file-watch-ignored-directories)

            (setq lsp-use-plists t)

            (with-eval-after-load 'lsp-mode
              (add-to-list 'lsp-file-watch-ignored-directories "bazel-[^/\\\\]+\\'")
              (add-to-list 'lsp-file-watch-ignored-directories "output[^/\\\\]+\\'")))
  :hook ((interactive-haskell-mode . subword-mode)
         (interactive-haskell-mode . company-mode)
         (interactive-haskell-mode . company-quickhelp-mode)
         (interactive-haskell-mode . smartparens-strict-mode)
         (interactive-haskell-mode . lsp-deferred)
         (haskell-mode . yas-minor-mode)
         (interactive-haskell-mode . haskell-hoogle-start-server)
         ;; (interactive-haskell-mode . my-haskell-mode-hook)
         (haskell-literate-mode . lsp-deferred)
         (lsp-after-initialize . lsp-ui-mode)
         (lsp-after-initialize . lsp-enable-which-key-integration)
         ;; (interactive-haskell-mode . flyspell-prog-mode)
         ;; (interactive-haskell-mode . haskell-auto-insert-module-template)
         )
  :bind (:map interactive-haskell-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references)
              ("C-c i" . lsp-ui-imenu)
              :map lsp-ui-mode-map
              ("C-c h" . lsp-ui-doc-show))

  :custom
  (gc-cons-threshold 100000000)
  (lsp-haskell-formatting-provider "stylish-haskell")
  ;; (lsp-haskell-plugin-tactics-global-on nil)
  ;; (lsp-haskell-plugin-haddock-comments-global-on nil)
  ;; (lsp-haskell-plugin-stan-global-on nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-max-height 50)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-flycheck-list-position 'top)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-peek-enable t)
  (haskell-mode-hook '(interactive-haskell-mode
                       capitalized-words-mode
                       haskell-decl-scan-mode
                       ;; haskell-indent-mode
                       haskell-indentation-mode
                       highlight-uses-mode
                       ;; turn-on-haskell-unicode-input-method
                       ))
  (haskell-process-suggest-hoogle-imports nil)
  (haskell-hoogle-server-command (lambda (port) (list "stack" "hoogle" "--" "server" "--local" "--port" (number-to-string port))))
  (haskell-interactive-popup-errors nil)

  :ensure t
  :ensure smartparens
  :ensure haskell-mode
  :ensure company
  :ensure company-ghci
  :ensure company-cabal
  :ensure company-quickhelp
  :ensure treemacs
  :ensure treemacs-all-the-icons
  :ensure lsp-mode
  :ensure lsp-ui
  :ensure lsp-haskell
  :ensure which-key)

;; (use-package haskell-mode
;;   :config (progn
;;             (require 'xref)
;;             (require 'eglot)
;;             (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp"))))

;;   :hook ((haskell-mode . eglot-ensure)
;;          (haskell-mode . yas-minor-mode)
;;          (haskell-mode . company-mode)
;;          ;; (haskell-mode . (lambda () (call-interactively #'xref-etags-mode)))
;;          )

;;   :custom
;;   (eglot-autoshutdown t) ;; shutdown language server after closing last file
;;   (eglot-confirm-server-initiated-edits nil) ;; allow edits without confirmation
;;   (eglot-extend-to-xref t)

;;   :ensure t
;;   :ensure eglot)

;; (cl-defun citre-peek-jump-abort ()
;;   (interactive)
;;   (citre-peek-jump)
;;   (citre-peek-abort))

;; (use-package haskell-mode
;;   ;; :bind (:map interactive-haskell-mode-map
;;   ;;             ("M-." . citre-peek)
;;   ;;             ;; ("M->" . citre-jump-back)
;;   ;;             ("M-/" . company-capf)
;;   ;;             :map citre-peek-keymap
;;   ;;             ("M-." . citre-peek-jump-abort))
;;   :config (progn
;;             (require 'citre)
;;             (require 'citre-config)
;;             (require 'company)
;;             (require 'company-capf))
;;   :hook ((haskell-mode . interactive-haskell-mode)
;;          (interactive-haskell-mode . citre-mode)
;;          (interactive-haskell-mode . company-mode)
;;          (interactive-haskell-mode . company-quickhelp-mode)
;;          (interactive-haskell-mode . company-statistics-mode)
;;          (interactive-haskell-mode . smartparens-strict-mode)
;;          )

;;   :ensure t
;;   :ensure citre
;;   :ensure company)

(provide 'init-haskell)
