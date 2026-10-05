;;; mijn-package-policy.el --- Package archive and update policy -*- lexical-binding: t; -*-

(require 'package)

(defconst mijn-package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("rails-to-cosmos" . "https://rails-to-cosmos.github.io/elpa/"))
  "Package archives used by interactive Emacs and background checks.")

(defun mijn-configure-package-archives ()
  "Apply the configured package archive policy."
  (setq package-archives mijn-package-archives))

(defun mijn-package-refresh-upgrade-count ()
  "Refresh package metadata and return the number of available upgrades."
  (mijn-configure-package-archives)
  (package-initialize)
  (package-refresh-contents)
  (length (package--upgradeable-packages)))

(provide 'mijn-package-policy)
;;; mijn-package-policy.el ends here
