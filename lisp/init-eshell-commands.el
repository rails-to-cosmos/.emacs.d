(defun spawn-shell (name &rest commands)
  "Invoke shell with commands"
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create name))
  (shell (current-buffer))
  (loop for command in commands
        do (process-send-string nil (concat command "\n")))
  (goto-char (point-max)))

;; nginx-administration

(defun nginx--reload ()
  (interactive)
  (spawn-shell "*nginx-administration*" "sudo nginx -s reload"))

(defun nginx--stop ()
  (interactive)
  (spawn-shell "*nginx-administration*" "sudo nginx -s stop"))

(defun nginx--start ()
  (interactive)
  (spawn-shell "*nginx-administration*" "sudo nginx -s start"))

;; apache-administration

(defun apache--restart ()
  (interactive)
  (spawn-shell "*apache-administration*" "sudo apachectl -k restart"))

(defun apache--stop ()
  (interactive)
  (spawn-shell "*apache-administration*" "sudo apachectl stop"))

(defun apache--start ()
  (interactive)
  (spawn-shell "*apache-administration*" "sudo apachectl start"))

(defun acqp--mychild-auth ()
  (interactive)
  (spawn-shell "*acqp--mychild-api*"
               "curl -H 'Accept: application/json' \\
                     -X POST 'https://mychild.acquiropay.ru/api/authenticate' \\
                     -d 'username=9263432275&password=qwadzv&uniqueDeviceId=123&osType=iOS&osVersion=6.1.4&appVersion=1' | jq '.sessionId'"))

(provide 'init-eshell-commands)
