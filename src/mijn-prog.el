;;; mijn-prog.el --- Shared baseline for programming modes -*- lexical-binding: t; -*-

(require 'prog-mode)
(require 'direnv)

(defun mijn-prog-setup ()
  "Enable the tooling common to every programming buffer.
Language modules layer only their major-mode and LSP specifics on top of
this: strict paren editing, in-buffer completion, snippet expansion, subword
navigation, and per-project environment activation (direnv/mise/venv)."
  (smartparens-strict-mode)
  (company-mode)
  (yas-minor-mode)
  (subword-mode)
  (direnv))

(add-hook 'prog-mode-hook #'mijn-prog-setup)

(provide 'mijn-prog)
;;; mijn-prog.el ends here
