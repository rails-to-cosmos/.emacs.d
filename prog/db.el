;;; db.el --- my database clients
;;
;; Filename: db.el
;; Description: my database clients
;; Author: Dmitry Akatov
;; Created: <2016-10-24 Mon 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d/prog/db.el
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

(defun init-db () "Initialize db with my configuration." (interactive))

(setq-default sqlplus-session-cache-dir (concat user-emacs-directory "sqlplus-session"))

(defun sqp-connect ()
  "Manage sqp connections."
  (interactive)
  (defvar sqp-connections)
  (defvar sqp-connections-names)
  (defvar sqp-connection-name)
  (defvar sqp-connection-string)
  (setq sqp-connections-names (list))
  (mapc (lambda (key) (add-to-list 'sqp-connections-names (car key))) sqp-connections)
  (setq sqp-connection-name (ido-completing-read "Choose sqp connection: " sqp-connections-names))
  (setq sqp-connection-string (cdr (assoc sqp-connection-name sqp-connections)))
  (sqlplus sqp-connection-string sqp-connection-name))

(provide 'db)
;;; db.el ends here
