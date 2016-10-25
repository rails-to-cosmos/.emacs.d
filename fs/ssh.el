
;;; ssh.el --- manage ssh connections
;;
;; Filename: ssh.el
;; Description: manage ssh connections
;; Author: Dmitry Akatov
;; Created: <2016-10-24 Mon 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d/fs/ssh.el
;; Keywords: Emacs 24.5
;; Compatibility: emacs >= 24.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun init-ssh () "Initialize ssh with my configuration." (interactive))

(defun ssh-connect (foreign-buffer)
  "Manage ssh connections, if FOREIGN-BUFFER > 1, do not reuse eshell."
  (interactive "p")
  (defvar ssh-connections)
  (defvar ssh-connections-names)
  (defvar ssh-connection-name)
  (defvar ssh-connection-string)
  (setq ssh-connections-names (list))
  (mapc (lambda (key) (add-to-list 'ssh-connections-names (car key))) ssh-connections)
  (setq ssh-connection-name (ido-completing-read "Choose ssh connection: " ssh-connections-names))
  (setq ssh-connection-string (cdr (assoc ssh-connection-name ssh-connections)))
  (let ((default-directory ssh-connection-string)
        (_buffn (get-buffer ssh-connection-name)))
    (if (or (eq nil _buffn) (> foreign-buffer 1))
          (progn
            (eshell t)
            (rename-buffer ssh-connection-name t))
        (progn
          (switch-to-buffer _buffn)))))

(provide 'ssh)
;;; ssh.el ends here
