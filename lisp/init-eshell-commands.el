(defun spawn-shell (name &rest commands)
  "Invoke shell with commands"
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create name))
  (shell (current-buffer))
  (loop for command in commands
        do (process-send-string nil (concat command "\n"))))

(defun nginx--reload ()
  (interactive)
  (spawn-shell "*nginx-administration*" "sudo nginx -s reload"))

(defun nginx--stop ()
  (interactive)
  (spawn-shell "*nginx-administration*" "sudo nginx -s stop"))

(defun nginx--start ()
  (interactive)
  (spawn-shell "*nginx-administration*" "sudo nginx -s start"))

(defun apache--restart ()
  (interactive)
  (spawn-shell "*nginx-administration*" "sudo apachectl -k restart"))

(defun apache--stop ()
  (interactive)
  (spawn-shell "*apache-administration*" "sudo apachectl stop"))

(defun apache--start ()
  (interactive)
  (spawn-shell "*apache-administration*" "sudo apachectl start"))

(provide 'init-eshell-commands)
