;;; xmpp.el --- my xmpp settings
;;
;; Filename: xmpp.el
;; Description: my xmpp settings
;; Author: Dmitry Akatov
;; Created: <2016-10-19 Wed 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/chat/xmpp.el
;; Keywords: Emacs 24.5
;; Compatibility: emacs >= 24.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package jabber
  :config (progn
            (setq jabber-history-enabled t
                  jabber-use-global-history nil
                  jabber-backlog-number 40
                  jabber-backlog-days 30)

            (defun jabber-init ()
              "Initialize jabber with my configuration"
              (interactive)

              (use-package notify)

              ;; Message alert hooks
              (define-jabber-alert echo "Show a message in the echo area"
                (lambda (msg)
                  (unless (minibuffer-prompt)
                    (message "%s" msg))))

              (defun notify-jabber-notify (from buf text proposed-alert)
                "(jabber.el hook) Notify of new Jabber chat messages via notify.el"
                (when (or jabber-message-alert-same-buffer
                          (not (memq (selected-window) (get-buffer-window-list buf))))
                  (if (jabber-muc-sender-p from)
                      (notify (format "(PM) %s"
                                      (jabber-jid-displayname (jabber-jid-user from)))
                              (format "%s: %s" (jabber-jid-resource from) text)))
                  (notify (format "%s" (jabber-jid-displayname from))
                          text)))

              (add-hook 'jabber-alert-message-hooks 'notify-jabber-notify)

              (setq jabber-chat-header-line-format
                    '(" " (:eval (jabber-jid-displayname jabber-chatting-with))
                      " " (:eval (jabber-jid-resource jabber-chatting-with)) "\t";
                      (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
                               (propertize
                                (or
                                 (cdr (assoc (get buddy 'show) jabber-presence-strings))
                                 (get buddy 'show))
                                'face
                                (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
                                    'jabber-roster-user-online))))
                      "\t" (:eval (get (jabber-jid-symbol jabber-chatting-with) 'status))
                      (:eval (unless (equal "" *jabber-current-show*)
                               (concat "\t You're " *jabber-current-show*
                                       " (" *jabber-current-status* ")"))))))

            (jabber-connect-all))
  :ensure t)

(provide 'xmpp)
;;; xmpp.el ends here
