;;; mijn-typescript.el --- TypeScript and JavaScript-family setup -*- lexical-binding: t; -*-

(up typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.mts\\'" . typescript-mode)
         ("\\.cts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook ((typescript-mode . eglot-ensure))
  :config
  (setq typescript-indent-level 2)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((typescript-mode js-mode) . ("typescript-language-server" "--stdio"))))
  :ensure nil
  :ensure typescript-mode
  :ensure eglot)

;; Built-in `js-mode' covers the JavaScript family.
(add-to-list 'auto-mode-alist '("\\.[mc]?js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.json[c5]?\\'" . js-json-mode))
(add-hook 'js-mode-hook #'eglot-ensure)
(setq js-indent-level 2)

(provide 'mijn-typescript)
;;; mijn-typescript.el ends here
