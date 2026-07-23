;;; mijn-email.el --- Email stack loader (mu4e + msmtp) -*- lexical-binding: t; -*-

;; Entry point for the email workflow. See email-proposal.md for design.
;;
;; External requirements (install via src/email/setup-email.sh):
;;   - isync  (mbsync)      — IMAP → Maildir sync
;;   - mu     (mu + mu4e)   — index + Emacs client
;;   - msmtp                — SMTP sender
;;   - pass + gnupg         — credential storage
;;
;; After the external side is set up:
;;   (add-to-list 'load-path (expand-file-name "src/email" user-emacs-directory))
;;   (require 'mijn-email)
;;   M-x mu4e

(defgroup mijn-email nil
  "Local-first email in Emacs via mu4e + mbsync + msmtp."
  :group 'mail
  :prefix "mijn-email-")

(defcustom mijn-email-user-full-name "Dmitry Akatov"
  "Display name used in outgoing mail's From: header."
  :type 'string
  :group 'mijn-email)

(defcustom mijn-email-user-address "akatovda@gmail.com"
  "Primary address used for mu init and outgoing mail."
  :type 'string
  :group 'mijn-email)

(defcustom mijn-email-maildir "~/Maildir"
  "Root of the local Maildir tree synced by mbsync."
  :type 'directory
  :group 'mijn-email)

(defcustom mijn-email-fetch-command "mbsync -a"
  "Shell command mu4e invokes to fetch new mail.
Runs asynchronously; configure `mu4e-update-interval' for cadence."
  :type 'string
  :group 'mijn-email)

(defcustom mijn-email-update-interval 300
  "Seconds between background `mbsync' fetches. Nil disables auto-update."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'mijn-email)

(require 'mijn-email-mu4e)
(require 'mijn-email-smtp)

;;;###autoload
(defun mijn-email ()
  "Open mu4e (the main inbox view)."
  (interactive)
  (require 'mu4e)
  (mu4e))

(global-set-key (kbd "C-x y m") #'mijn-email)

(provide 'mijn-email)
;;; mijn-email.el ends here
