;;; ssh.el --- manage ssh connections
;;; Commentary:
;;; Code:

(defun init-ssh () "Initialize ssh with my configuration." (interactive))

(defun keys (inlist)
  "Return keys of INLIST."
  (let ((alkeys (list)))
    (mapc (lambda (key) (add-to-list 'alkeys (car key))) inlist)
    alkeys))

(defun open-ssh-connection (ssh-connection-name)
  (let ((default-directory ssh-connection-string)
        (ssh-buffer-name (concat "*" ssh-connection-name "-shell*")))
    (setq ssh-connection-string (cdr (assoc ssh-connection-name ssh-connections)))
    (if (eq nil (get-buffer ssh-buffer-name))
        (progn
          (eshell t)
          (rename-buffer ssh-buffer-name t))
      (progn
        (switch-to-buffer (get-buffer ssh-buffer-name))))))

(defun ssh-connect ()
  "Manage ssh connections, if FOREIGN-BUFFER > 1, do not reuse eshell."
  (interactive)
  (defvar ssh-connections)
  (defvar ssh-connections-names)
  (defvar ssh-connection-name)

  (setq ssh-connections-names (list))
  (mapc (lambda (key) (add-to-list 'ssh-connections-names (car key))) ssh-connections)
  (setq ssh-connection-name (ido-completing-read "Choose ssh connection: " ssh-connections-names))
  (open-ssh-connection ssh-connection-name))

(provide 'ssh)
;;; ssh.el ends here
