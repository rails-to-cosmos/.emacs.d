;;; early-init.el --- Explicit package activation -*- lexical-binding: t; -*-

;; init.el loads Transient before activating packages that use it in autoloads.
(setq package-enable-at-startup nil)
