(defun spawn-shell (name &rest commands)
  "Invoke shell with commands"
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create name))
  (setq eshell-buffer-name name)
  (eshell)
  (loop for command in commands
        do (insert (concat command "\n")))
  (eshell-send-input)
  (goto-char (point-max)))

;; chordials administration

(defvar chordials-root-dir "~/Documents/Stuff/Chordials/")
(defvar chordials-api-dir "~/Documents/Stuff/Chordials/api/")
(defvar chordials-env-dir (concatenate 'string chordials-root-dir "env/"))
(defvar chordials-env-bin-dir (concatenate 'string chordials-env-dir "bin/"))
(defvar chordials-python (concatenate 'string chordials-env-dir "bin/python3"))
(defvar chordials-pip (concatenate 'string chordials-env-dir "bin/pip"))

(defun chordials-api-run ()
  (interactive)
  (spawn-shell "*Chordials Shell*" (concatenate 'string "cd " chordials-root-dir))
  (spawn-shell "*Chordials BaseX Server Listener*" "basexhttp")
  (spawn-shell "*Chordials Server Listener*" (concatenate 'string chordials-env-bin-dir "python3 " chordials-api-dir "api.py"))
  (sanityinc/toggle-delete-other-windows))

(defun chordials-api-kill ()
  (interactive)
  (shell-command "basexhttpstop")
  (kill-buffer "*Chordials BaseX Server Listener*")
  (kill-buffer "*Chordials Shell*")
  (kill-buffer "*Chordials Server Listener*")
  (sanityinc/toggle-delete-other-windows))

(global-set-key (kbd "C-x C-y C-c C-a C-r") 'chordials-api-run)
(global-set-key (kbd "C-x C-y C-c C-a C-k") 'chordials-api-kill)

(defun chordials-pip-install (name)
  "Install pip package"
  (interactive "MName of pip package to install: ")
  (spawn-shell "*Chordials Pip Manager Shell*" (concatenate 'string chordials-pip " install " name)))

(defun chordials-pip-update ()
  "Update pip packages"
  (interactive)
  (spawn-shell "*Chordials Pip Manager Shell*" (concatenate 'string chordials-pip " freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 " chordials-pip " install -U")))

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
