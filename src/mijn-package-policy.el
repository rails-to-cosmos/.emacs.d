;;; mijn-package-policy.el --- Package archive and update policy -*- lexical-binding: t; -*-

(require 'package)

(defconst mijn-package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ;; Retained for packages unavailable elsewhere (e.g. vterm, ox-reveal).
    ;; Low priority still permits new dependencies from this unsigned source.
    ;; Remove once these packages are replaced, dropped, or pinned and reviewed
    ;; in packages/ or rails-to-cosmos.
    ("melpa-unstable" . "https://melpa.org/packages/")
    ("rails-to-cosmos" . "https://rails-to-cosmos.github.io/elpa/"))
  "Package archives used by interactive Emacs and background checks.")

(defun mijn-configure-package-archives ()
  "Apply the configured package archive policy."
  (setq package-archives mijn-package-archives
        package-archive-priorities '(("rails-to-cosmos" . 100) ("gnu" . 50)
                                    ("nongnu" . 40) ("melpa-stable" . 20)
                                    ("melpa-unstable" . 0))
        ;; Require a valid signature; archives also carry expired co-signatures.
        package-check-signature t
        package-unsigned-archives '("melpa-stable" "melpa-unstable"
                                    "rails-to-cosmos"))
  ;; Stable 0.1.0 requires company-elisp, removed from current Company.
  (setf (alist-get 'company-eask package-pinned-packages) "melpa-unstable")
  ;; Stable 1.2.0 calls treemacs-initialize without its now-required extension.
  (setf (alist-get 'lsp-metals package-pinned-packages) "melpa-unstable"))

(defun mijn-package--refuse-validation-install (&rest _)
  "Stop batch validation before an unstaged package can execute on the host."
  (message "Package validation cannot install packages; stage missing packages first")
  (kill-emacs 1))

(defun mijn-package-initialize (&optional dependencies-only)
  "Initialize packages with Transient available to generated autoloads.
DEPENDENCIES-ONLY leaves unrelated packages inactive during staged installs."
  (when (and noninteractive (equal (getenv "EMACS_PACKAGE_VALIDATE_ONLY") "1"))
    (dolist (function '(package-install package-install-file
                        package-install-from-archive package-vc-install))
      (advice-add function :before #'mijn-package--refuse-validation-install)))
  (package-initialize t)
  (package-activate 'transient)
  (require 'transient)
  (unless dependencies-only (package-activate-all)))

(defun mijn-package-refresh-upgrade-count ()
  "Refresh package metadata and return the number of available upgrades."
  (mijn-configure-package-archives)
  (mijn-package-initialize)
  (package-refresh-contents)
  (length (package--upgradeable-packages)))

(provide 'mijn-package-policy)
;;; mijn-package-policy.el ends here
