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
                  jabber-use-global-history t
                  jabber-backlog-number 40
                  jabber-backlog-days 30
                  jabber-chat-buffer-show-avatar nil
                  jabber-vcard-avatars-publish nil
                  jabber-chat-fill-long-lines nil
                  jabber-roster-show-title nil
                  jabber-chatstates-confirm nil
                  jabber-vcard-avatars-retrieve nil
                  jabber-roster-line-format "%c %-25n %u %-8s  %S"
                  jabber-auto-reconnect t
                  jabber-avatar-verbose nil
                  jabber-chat-buffer-format "%n"
                  jabber-history-size-limit 1024000000
                  jabber-show-resources nil
                  jabber-roster-show-bindings nil
                  jabber-show-offline-contacts t
                  jabber-chat-local-prompt-format "[%t] [me]: ")

            (defvar my-jabber-input-history '() "Variable that holds input history")
            (make-variable-buffer-local 'my-jabber-input-history)

            (defvar my-jabber-input-history-position 0 "Current position in input history")
            (make-variable-buffer-local 'my-jabber-input-history-position)

            (defvar my-jabber-input-history-current nil "Current input value")
            (make-variable-buffer-local 'my-jabber-input-history-current)

            (defun my-jabber-input-history-hook (body id)
              (add-to-list 'my-jabber-input-history body t)
              (setq my-jabber-input-history-position (length my-jabber-input-history)))
            (add-hook 'jabber-chat-send-hooks 'my-jabber-input-history-hook)

            (defun my-jabber-previous-input ()
              (interactive)
              (let (current-input (pos my-jabber-input-history-position) (len (length my-jabber-input-history)))
                (if (= pos 0)
                    (message "%s" "No previous input")
                  (setq current-input (delete-and-extract-region jabber-point-insert (point-max)))
                  (when (= pos len) ; running first time, save current input
                    (setq my-jabber-input-history-current current-input))
                  (decf my-jabber-input-history-position)
                  (insert (nth my-jabber-input-history-position my-jabber-input-history)))))

            (defun my-jabber-next-input ()
              (interactive)
              (let ((pos my-jabber-input-history-position) (len (length my-jabber-input-history)))
                (cond
                 ((= pos (1- len)) ; pointing at the last element, insert saved input
                  (incf my-jabber-input-history-position)
                  (delete-region jabber-point-insert (point-max))
                  (insert my-jabber-input-history-current)
                  (setq my-jabber-input-history-current nil))
                 ((= pos len)                              ; pointing beyound last element, notify user
                  (message "%s" "No next input"))
                 (t                                ; insert next history item
                  (incf my-jabber-input-history-position)
                  (delete-region jabber-point-insert (point-max))
                  (insert (nth my-jabber-input-history-position my-jabber-input-history))))))

            (define-key jabber-chat-mode-map (kbd "M-p") 'my-jabber-previous-input)
            (define-key jabber-chat-mode-map (kbd "M-n") 'my-jabber-next-input)

            (defun my-jabber-input-history-choose ()
              (interactive)
              (let ((choice (ido-completing-read "Select history item: " (reverse my-jabber-input-history))))
                (delete-region jabber-point-insert (point-max))
                (insert choice)))

            (defun jabber-init ()
              "Initialize jabber with my configuration"
              (interactive)
              (jabber-connect-all)
              (jabber-display-roster)))
  :ensure t)

(provide 'xmpp)
;;; xmpp.el ends here
