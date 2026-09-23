;;; mijn-packages.el --- Package declarations -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)
(require 'seq)
(require 'use-package)

;; These packages are needed before their `up' declarations can run.  All
;; other package requirements are registered by `up' itself.
(defconst mijn-bootstrap-packages '(transient use-package s table-view))

(defvar mijn-required-packages (copy-sequence mijn-bootstrap-packages)
  "Packages declared by `up', plus early bootstrap dependencies.")

(defvar mijn-vc-packages nil
  "Packages declared by `up' with a `:vc' recipe.")

(defvar mijn-package-archives-refreshed nil)

(defun mijn-sync-package-selected-packages ()
  "Expose the tracked package roots to package.el.
Keep selections made interactively or in `custom.el', while ensuring that
`package-autoremove' never mistakes configured packages for dependencies."
  (setq package-selected-packages
        (delete-dups
         (append mijn-required-packages
                 mijn-vc-packages
                 package-selected-packages))))

(defun mijn-register-packages (packages &optional vc-package)
  "Register and install PACKAGES declared by `up'.
VC-PACKAGE is left for use-package's `:vc' support to install."
  (dolist (package packages)
    (cl-pushnew package mijn-required-packages))
  (when vc-package
    (cl-pushnew vc-package mijn-vc-packages))
  (let ((missing (seq-filter
                  (lambda (package)
                    (and (not (eq package vc-package))
                         (not (package-installed-p package))))
                  packages)))
    (when (and missing (not mijn-package-archives-refreshed))
      (package-refresh-contents)
      (setq mijn-package-archives-refreshed t))
    (dolist (package missing)
      (ignore-errors (package-install package))))
  (mijn-sync-package-selected-packages))

(defmacro up (name &rest args)
  "Declare NAME with `use-package' and track every package it requires.
Repeated `:ensure' entries are supported because several declarations use
one block to provision related packages."
  (declare (indent 1))
  (let ((packages (list name))
        (rest args)
        vc)
    (while rest
      (let ((item (pop rest)))
        (cond
         ((eq item :vc) (setq vc t))
         ((eq item :ensure)
          (let ((value (pop rest)))
            (when (and (symbolp value)
                       value
                       (not (eq value t)))
              (cl-pushnew value packages)))))))
    `(progn
       (mijn-register-packages ',(nreverse packages) ,(and vc `',name))
       (use-package ,name ,@args))))

(provide 'mijn-packages)
;;; mijn-packages.el ends here
