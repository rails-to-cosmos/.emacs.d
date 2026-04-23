;;; mijn-email-mu4e.el --- mu4e configuration for Gmail via mbsync -*- lexical-binding: t; -*-

;; Folder layout matches src/email/setup-email.sh:
;;   ~/Maildir/gmail/{INBOX,Sent,Drafts,All,Trash,Spam}

(require 'cl-lib)

(declare-function mu4e "mu4e")
(declare-function make-mu4e-context "mu4e-context")

(defvar mu4e-maildir)
(defvar mu4e-mu-binary)
(defvar mu4e-get-mail-command)
(defvar mu4e-update-interval)
(defvar mu4e-change-filenames-when-moving)
(defvar mu4e-sent-messages-behavior)
(defvar mu4e-drafts-folder)
(defvar mu4e-sent-folder)
(defvar mu4e-refile-folder)
(defvar mu4e-trash-folder)
(defvar mu4e-maildir-shortcuts)
(defvar mu4e-bookmarks)
(defvar mu4e-view-show-images)
(defvar mu4e-view-prefer-html)
(defvar mu4e-compose-format-flowed)
(defvar mu4e-compose-signature)
(defvar mu4e-compose-dont-reply-to-self)
(defvar mu4e-confirm-quit)
(defvar mu4e-attachment-dir)
(defvar mu4e-headers-include-related)
(defvar mu4e-headers-skip-duplicates)
(defvar mu4e-completing-read-function)
(defvar mu4e-contexts)

(with-eval-after-load 'mu4e
  ;; Core paths and commands
  (setq mu4e-maildir (expand-file-name mijn-email-maildir)
        mu4e-mu-binary (or (executable-find "mu") "mu")
        mu4e-get-mail-command mijn-email-fetch-command
        mu4e-update-interval mijn-email-update-interval

        ;; mbsync renames files on sync — mu4e must rename-aware move
        mu4e-change-filenames-when-moving t

        ;; Gmail manages "Sent" via IMAP label, so don't also save locally
        mu4e-sent-messages-behavior 'delete

        ;; Search quality of life
        mu4e-headers-include-related t
        mu4e-headers-skip-duplicates t

        ;; Viewing / composing
        mu4e-view-show-images t
        mu4e-view-prefer-html nil
        mu4e-compose-format-flowed t
        mu4e-compose-dont-reply-to-self t
        mu4e-confirm-quit nil
        mu4e-attachment-dir "~/Downloads"

        ;; Use default completing-read (plays with vertico etc.)
        mu4e-completing-read-function #'completing-read)

  ;; Gmail folder shortcuts — reachable with `j` + letter in the headers view
  (setq mu4e-maildir-shortcuts
        '(("/gmail/INBOX"  . ?i)
          ("/gmail/Sent"   . ?s)
          ("/gmail/Drafts" . ?d)
          ("/gmail/All"    . ?a)
          ("/gmail/Trash"  . ?t)
          ("/gmail/Spam"   . ?S)))

  ;; Bookmarks live in the main mu4e view; hit `b <char>` to open.
  (setq mu4e-bookmarks
        '((:name  "Unread in Inbox"
           :query "maildir:/gmail/INBOX AND flag:unread AND NOT flag:trashed"
           :key   ?u)
          (:name  "Last 7 days"
           :query "date:7d..now AND NOT flag:trashed"
           :key   ?w)
          (:name  "Flagged"
           :query "flag:flagged"
           :key   ?f)
          (:name  "With attachments (30d)"
           :query "flag:attach AND date:30d..now"
           :key   ?a)))

  ;; Single-account setup — the context system is wired so adding a second
  ;; account is just another `make-mu4e-context' entry plus an IMAPAccount
  ;; block in ~/.mbsyncrc.
  (require 'mu4e-context)
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "gmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/gmail"
                               (mu4e-message-field msg :maildir))))
          :vars `((user-mail-address     . ,mijn-email-user-address)
                  (user-full-name        . ,mijn-email-user-full-name)
                  (mu4e-drafts-folder    . "/gmail/Drafts")
                  (mu4e-sent-folder      . "/gmail/Sent")
                  (mu4e-refile-folder    . "/gmail/All")
                  (mu4e-trash-folder     . "/gmail/Trash"))))))

;; Global compose binding — works even before mu4e has been visited.
(global-set-key (kbd "C-x m") #'mu4e-compose-new)

(provide 'mijn-email-mu4e)
;;; mijn-email-mu4e.el ends here
