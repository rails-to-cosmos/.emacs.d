;;; package-setup.el --- Archive policy for component installation -*- lexical-binding: t; -*-

(load (expand-file-name "src/mijn-package-policy.el" user-emacs-directory) nil t)
(mijn-configure-package-archives)
