;;; mijn-c.el --- C/C++ development setup -*- lexical-binding: t; -*-

;; Source navigation strategy:
;;   clangd (eglot, M-.)  — declarations, headers, in-project definitions
;;   ggtags (GNU Global)  — implementations across library sources
;;
;; For a project, run init.sh to:
;;   - fetch library sources into .sources/
;;   - build GTAGS databases per library
;; Then wire GTAGSLIBPATH (in .envrc or .dir-locals.el) so ggtags spans them.

(up disaster
  :ensure t)

(up highlight-doxygen
  :ensure nil
  :hook ((c-mode . highlight-doxygen-mode)
         (c++-mode . highlight-doxygen-mode)))

(up cmake-mode
  :ensure nil
  :hook (cmake-mode . eglot-ensure))

(up cmake-font-lock
  :ensure t)

;; GNU Global — cross-project source navigation (M-. into library implementations).
;; Set GTAGSLIBPATH in .envrc or .dir-locals.el to also search library sources:
;;   (setenv "GTAGSLIBPATH" "/path/to/project/.sources/raylib:/usr/src/glibc/glibc-2.40")
(up ggtags
  :ensure nil
  :hook ((c-mode   . ggtags-mode)
         (c++-mode . ggtags-mode)
         (asm-mode . ggtags-mode)))

(up cc-mode
  :config
  (add-hook 'c-mode-common-hook (lambda () (electric-indent-local-mode -1)))
  (keymap-set c-mode-map "<Return>" #'electric-newline-and-maybe-indent)
  (keymap-set c++-mode-map "<Return>" #'electric-newline-and-maybe-indent)

  :custom
  (c-basic-offset 4)

  :hook (((c-mode c++-mode objc-mode) . eglot-ensure))

  :bind (:map c-mode-map
              ("C-x C-x" . ff-find-other-file)
         :map c++-mode-map
              ("C-x C-x" . ff-find-other-file))

  :ensure nil
  :ensure eglot
  :ensure company
  :ensure yasnippet)

(provide 'mijn-c)
;;; mijn-c.el ends here
