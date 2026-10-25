;;; mijn-swift.el --- Swift development setup -*- lexical-binding: t; -*-

(require 'eglot)

(defun mijn-swift-eglot-command (_interactive)
  "Use the selected Apple toolchain locally, or SourceKit-LSP on PATH."
  (if (and (eq system-type 'darwin) (not (file-remote-p default-directory)))
      '("/usr/bin/xcrun" "sourcekit-lsp")
    '("sourcekit-lsp")))

(defun mijn-swift-setup ()
  "Configure Swift indentation and start language support."
  (setq-local indent-tabs-mode nil
              tab-width 4)
  (eglot-ensure))

(up swift-mode
  :ensure t
  :ensure eglot
  :mode "\\.swift\\'"
  :hook (swift-mode . mijn-swift-setup)
  :custom (swift-mode:basic-offset 4)
  :bind (:map swift-mode-map
              ("C-c C-f" . eglot-format-buffer)
              ("C-c C-r" . eglot-rename)
              ("C-c C-a" . eglot-code-actions)
              ("C-c C-c" . compile))
  :init
  (add-to-list 'eglot-server-programs '(swift-mode . mijn-swift-eglot-command)))

(provide 'mijn-swift)
;;; mijn-swift.el ends here
