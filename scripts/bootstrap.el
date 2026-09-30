;;; bootstrap.el --- Provision and verify this checkout -*- lexical-binding: t; -*-

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory load-file-name))))
(unless (and (version<= "29.1" emacs-version) module-file-suffix)
  (error "Use Emacs >= 29.1 built with dynamic module support"))

(load (expand-file-name "test/integration/load-check.el" user-emacs-directory)
      nil nil t)

(require 'vterm)
(unless (featurep 'vterm-module)
  (error "The vterm native module did not load"))
(let ((vterm-shell "/bin/sh")
      (vterm-kill-buffer-on-exit nil))
  (unwind-protect
      (progn
        (vterm "*bootstrap-vterm*")
        (vterm-send-string "printf 'BOOTSTRAP_%s\\n' 'VTERM_OK'")
        (vterm-send-return)
        (let ((deadline (+ (float-time) 10)))
          (while (and (< (float-time) deadline)
                      (not (string-match-p "BOOTSTRAP_VTERM_OK" (buffer-string))))
            (accept-process-output nil 0.1)))
        (unless (and (process-live-p vterm--process)
                     (string-match-p "BOOTSTRAP_VTERM_OK" (buffer-string)))
          (error "vterm shell verification failed: %s" (buffer-string)))
        (message "OK: vterm native module and shell work"))
    (when-let ((buffer (get-buffer "*bootstrap-vterm*")))
      (with-current-buffer buffer
        (when (processp vterm--process)
          (set-process-query-on-exit-flag vterm--process nil)))
      (kill-buffer buffer))))
