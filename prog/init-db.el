;;; init-db.el --- my database clients
;;; Commentary:
;;; Code:

(defvar sqp-connections)

(defun sqp-connect ()
  "Manage sqp connections."
  (interactive)
  (let ((sqp-connections-names (list)))
    (mapc (lambda (key) (add-to-list 'sqp-connections-names (car key))) sqp-connections)
    (let ((sqp-connection-name (ido-completing-read "Choose sqp connection: " sqp-connections-names)))
      (let ((sqp-connection-string (cdr (assoc sqp-connection-name sqp-connections))))
        (sqlplus sqp-connection-string sqp-connection-name)))))

(provide 'init-db)
;;; init-db.el ends here
