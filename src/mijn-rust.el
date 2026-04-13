;;; mijn-rust.el --- Rust development setup -*- lexical-binding: t; -*-

(defun mijn-rust--find-cargo-toml ()
  "Find nearest Cargo.toml from current file."
  (when-let ((dir (locate-dominating-file default-directory "Cargo.toml")))
    (expand-file-name "Cargo.toml" dir)))

(defun mijn-rust--eglot-workspace-config (_server)
  "Return rust-analyzer workspace config with linkedProjects."
  (if-let ((cargo (mijn-rust--find-cargo-toml)))
      `(:rust-analyzer (:linkedProjects [,cargo]))
    `(:rust-analyzer (:linkedProjects []))))

(use-package rust-mode
  :hook ((rust-mode . eglot-ensure)
         (rust-mode . company-mode)
         (rust-mode . smartparens-strict-mode)
         (rust-mode . yas-minor-mode))
  :config
  (setq rust-format-on-save t)
  (setq-default eglot-workspace-configuration #'mijn-rust--eglot-workspace-config)
  :ensure t
  :ensure eglot
  :ensure smartparens
  :ensure company
  :ensure yasnippet)

(provide 'mijn-rust)
;;; mijn-rust.el ends here
