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
        package-check-signature 'all
        package-unsigned-archives '("melpa-stable" "melpa-unstable"
                                    "rails-to-cosmos")))

(defun mijn-package-refresh-upgrade-count ()
  "Refresh package metadata and return the number of available upgrades."
  (mijn-configure-package-archives)
  (package-initialize)
  (package-refresh-contents)
  (length (package--upgradeable-packages)))

(provide 'mijn-package-policy)
;;; mijn-package-policy.el ends here
