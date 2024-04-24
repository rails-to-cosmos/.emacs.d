(require 'lsp)
(require 'lsp-mode)
(require 'dap-cpptools)

(use-package clang-capf
  :ensure t)

(use-package doxy-graph-mode
  :ensure t)

(use-package highlight-doxygen
  :ensure t)

(use-package cmake-mode
  :hook ((cmake-mode . lsp-deferred))
  :ensure t)

(use-package cmake-font-lock
  :ensure t)

(use-package cmake-ide
  :ensure t)

(defun !lsp-tramp-connection ()
  (set-process-coding-system proc 'binary 'utf-8-unix))

(use-package lsp-mode
  :commands lsp
  :config (progn
            (advice-add #'lsp-tramp-connection :override #'!lsp-tramp-connection)

            (setq inhibit-eol-conversion t))
  :ensure t)

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t)

;; (use-package ccls
;;   :ensure t
;;   :config (progn
;;             (setq ccls-executable "ccls")
;;             (setq lsp-prefer-flymake nil)
;;             (setq c-basic-offset 4)
;;             (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;             (ccls-use-default-rainbow-sem-highlight))
;;   :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp-deferred))))

(use-package cc-mode
  :config (progn
            (require 'dap-cpptools)

            (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'"))
  :custom
  (lsp-prefer-flymake nil)
  (c-basic-offset 4)
  :hook (((c-mode c++-mode objc-mode) . lsp-deferred)
         ((c-mode c++-mode objc-mode) . smartparens-strict-mode))
  :bind (("C-x C-x" . ff-find-other-file))
  :ensure t)

;; (use-package cc-mode
;;   :config (progn
;;             (require 'eglot))

;;   :custom
;;   (c-basic-offset 4)

;;   :hook (((c-mode c++-mode objc-mode) . eglot-ensure)
;;          ((c-mode c++-mode objc-mode) . smartparens-strict-mode))

;;   :bind (("C-x C-x" . ff-find-other-file))

;;   :ensure t)

(provide 'init-c)
