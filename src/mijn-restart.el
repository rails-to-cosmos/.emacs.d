;;; mijn-restart.el --- Restart session services from Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Pick a service (xmonad, emacs server, xmobar, ...) via `completing-read'
;; and restart it.  Emacs-native replacement for scripts/restart-menu.sh.
;; Shell targets run asynchronously and report their exit status; the emacs
;; server restarts in-process (no self-kill).

;;; Code:

(require 'server)

(defvar mijn-restart-alist
  '(("xmonad"       . "xmonad --recompile && xmonad --restart")
    ("xmobar status" . "pkill -f '[x]mobar-status daemon'")
    ("emacs server"  . mijn-restart--emacs-server))
  "Alist of (LABEL . ACTION).
ACTION is either a shell command string (run asynchronously) or a
function of no arguments.")

(defun mijn-restart--emacs-server ()
  "Restart the Emacs server socket in-process."
  (when (server-running-p)
    (server-force-delete))
  (server-start)
  (message "mijn-restart: emacs server restarted"))

(defun mijn-restart--shell (label command)
  "Run COMMAND asynchronously; report its result under LABEL."
  (let* ((buffer (get-buffer-create (format " *mijn-restart: %s*" label)))
         (process (start-process-shell-command
                   (format "mijn-restart/%s" label) buffer command)))
    (message "mijn-restart: %s ..." label)
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((code (process-exit-status proc)))
           (message "mijn-restart: %s %s"
                    label
                    (if (zerop code)
                        "ok"
                      (format "FAILED (exit %d)" code)))))))))

;;;###autoload
(defun mijn-restart (&optional label)
  "Pick a service by LABEL and restart it.
Interactively, prompt with `completing-read' over `mijn-restart-alist'."
  (interactive
   (list (completing-read "Restart: "
                          (mapcar #'car mijn-restart-alist)
                          nil t)))
  (let ((action (cdr (assoc label mijn-restart-alist))))
    (cond
     ((null action) (user-error "Unknown service: %s" label))
     ((functionp action) (funcall action))
     ((stringp action) (mijn-restart--shell label action))
     (t (user-error "Bad action for %s" label)))))

(provide 'mijn-restart)
;;; mijn-restart.el ends here
