;;; early-init.el --- Explicit package activation -*- lexical-binding: t; -*-

;; init.el loads Transient before activating packages that use it in autoloads.
(setq package-enable-at-startup nil)

(defvar mijn-startup-gc-cons-threshold gc-cons-threshold)

(defun mijn-restore-startup-gc ()
  "Restore normal garbage collection after startup."
  (setq gc-cons-threshold mijn-startup-gc-cons-threshold)
  (remove-hook 'emacs-startup-hook #'mijn-restore-startup-gc)
  (garbage-collect))

(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'emacs-startup-hook #'mijn-restore-startup-gc t)
