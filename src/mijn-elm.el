;;; mijn-elm.el --- Elm development setup -*- lexical-binding: t; -*-

(use-package elm-mode
  :hook ((elm-mode . eglot-ensure))
  :config
  ;; Run `elm-format' on save (elm-mode reads this defcustom).
  (setq elm-format-on-save t)
  ;; Map the Elm language server, in case this eglot predates its built-in entry.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(elm-mode . ("elm-language-server"))))
  ;; smartparens / company / yasnippet are wired globally by `mijn-prog';
  ;; keep only the Elm-specific setup here.
  :ensure nil
  :ensure elm-mode
  :ensure eglot
  :ensure smartparens
  :ensure company
  :ensure yasnippet)

(provide 'mijn-elm)
;;; mijn-elm.el ends here
