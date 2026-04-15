;;; mijn-c.el --- C/C++ development setup -*- lexical-binding: t; -*-

;; Source navigation strategy:
;;   clangd (eglot, M-.)  — declarations, headers, in-project definitions
;;   ggtags (GNU Global)  — implementations across library sources
;;
;; For a project, run init.sh to:
;;   - fetch library sources into .sources/
;;   - build GTAGS databases per library
;; Then wire GTAGSLIBPATH (in .envrc or .dir-locals.el) so ggtags spans them.

(use-package disaster
  :ensure t)

(use-package highlight-doxygen
  :ensure t
  :hook ((c-mode . highlight-doxygen-mode)
         (c++-mode . highlight-doxygen-mode)))

(use-package cmake-mode
  :ensure t
  :hook (cmake-mode . eglot-ensure))

(use-package cmake-font-lock
  :ensure t)

;; GNU Global — cross-project source navigation (M-. into library implementations).
;; Set GTAGSLIBPATH in .envrc or .dir-locals.el to also search library sources:
;;   (setenv "GTAGSLIBPATH" "/path/to/project/.sources/raylib:/usr/src/glibc/glibc-2.40")
(use-package ggtags
  :ensure t
  :hook ((c-mode   . ggtags-mode)
         (c++-mode . ggtags-mode)
         (asm-mode . ggtags-mode)))

(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook (lambda () (electric-indent-local-mode -1)))
  (keymap-set c-mode-map "<Return>" #'electric-newline-and-maybe-indent)
  (keymap-set c++-mode-map "<Return>" #'electric-newline-and-maybe-indent)

  :custom
  (c-basic-offset 4)

  :hook (((c-mode c++-mode objc-mode) . eglot-ensure)
         ((c-mode c++-mode objc-mode) . company-mode)
         ((c-mode c++-mode objc-mode) . smartparens-strict-mode)
         ((c-mode c++-mode objc-mode) . yas-minor-mode))

  :bind (:map c-mode-map
              ("C-x C-x" . ff-find-other-file)
         :map c++-mode-map
              ("C-x C-x" . ff-find-other-file))

  :ensure t
  :ensure eglot
  :ensure company
  :ensure yasnippet)

(provide 'mijn-c)
;;; mijn-c.el ends here
