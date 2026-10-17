;;; mijn-c.el --- C/C++ development setup -*- lexical-binding: t; -*-

(require 'eglot)

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
         (c-ts-mode . ggtags-mode)
         (c++-ts-mode . ggtags-mode)
         (asm-mode . ggtags-mode)))

(up cc-mode
  :config
  (let ((clangd (or (executable-find "clangd")
                    (let ((homebrew-clangd
                           (car (file-expand-wildcards
                                 "/opt/homebrew/opt/llvm/bin/clangd"))))
                      (and homebrew-clangd
                           (file-executable-p homebrew-clangd)
                           homebrew-clangd))
                    (let ((homebrew-clangd
                           (car (file-expand-wildcards
                                 "/usr/local/opt/llvm/bin/clangd"))))
                      (and homebrew-clangd
                           (file-executable-p homebrew-clangd)
                           homebrew-clangd)))))
    (when clangd
      (add-to-list 'eglot-server-programs
                   `((c-mode c++-mode objc-mode c-ts-mode c++-ts-mode) . (,clangd)))))
  (add-hook 'c-mode-common-hook (lambda () (electric-indent-local-mode -1)))
  (keymap-set c-mode-map "<Return>" #'electric-newline-and-maybe-indent)
  (keymap-set c++-mode-map "<Return>" #'electric-newline-and-maybe-indent)

  :custom
  (c-basic-offset 2)

  :hook (((c-mode c++-mode objc-mode c-ts-mode c++-ts-mode) . eglot-ensure)
         ((c-mode c++-mode c-ts-mode c++-ts-mode) . xref-etags-mode))

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
