(defun telegram-notify (message)
  (interactive "sMessage: ")

  (defvar telegram-notifier-bot-token)
  (unless (boundp 'telegram-notifier-bot-token)
    (let ((token (or
                  (getenv "EMACS_TELEGRAM_NOTIFIER_TOKEN")
                  (read-string "Telegram notifier bot token: "))))
      (customize-save-variable 'telegram-notifier-bot-token token)))

  (defvar telegram-chat-id)
  (unless (boundp 'telegram-chat-id)
    (let ((chat-id (or
                    (getenv "EMACS_TELEGRAM_NOTIFIER_CHAT_ID")
                    (read-string "Telegram chat id: "))))
      (customize-save-variable 'telegram-chat-id chat-id)))

  (start-process-shell-command
   "Notify" nil
   (expand-template
    (concat
     "curl "
     "--data chat_id={:chat_id} "
     "--data text=\"{:message}\" "
     "https://api.telegram.org/bot{:token}/sendMessage ")
    `(:chat_id ,telegram-chat-id
      :message ,message
      :token ,telegram-notifier-bot-token))))

(defun org-notify-action-telegram (plist)
  "Telegram notify action for org-notify."
  (let* ((title (plist-get plist :heading))
         (body (org-notify-body-text plist)))
    (telegram-notify (concat title "\n" body))))

(provide 'telegram-notify)
