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
                  jabber-backlog-days 30
                  jabber-chat-buffer-show-avatar nil
                  jabber-vcard-avatars-publish nil)

            (defun jabber-init ()
              "Initialize jabber with my configuration"
              (interactive))

            (jabber-connect-all))
  :ensure t)

; (set-face-foreground 'jabber-activity-personal-face "deep pink")
;; (set-face-bold-p 'jabber-activity-personal-face t)

;; (set-face-foreground 'jabber-chat-prompt-foreign "salmon")
;; (set-face-bold-p 'jabber-chat-prompt-foreign t)

;; (set-face-foreground 'jabber-chat-prompt-local "olive drab")
;; (set-face-bold-p 'jabber-chat-prompt-local t)

;; (set-face-foreground 'jabber-rare-time-face "LightGoldenrod3")
;; (set-face-underline-p 'jabber-rare-time-face t)

;; (set-face-foreground 'jabber-roster-user-away "peru")
;; (set-face-italic-p 'jabber-roster-user-away t)
;; (set-face-bold-p 'jabber-roster-user-away nil)

;; (set-face-foreground 'jabber-roster-user-online "dark orange")
;; (set-face-italic-p 'jabber-roster-user-online nil)
;; (set-face-bold-p 'jabber-roster-user-online t)

(setq jabber-roster-line-format "%c %-25n %u %-8s  %S")

(add-hook 'jabber-chat-mode-hook
          (lambda()
            (flyspell-mode t)
            (setq truncate-lines t)
            (setq word-wrap t)))


(provide 'xmpp)
;;; xmpp.el ends here
