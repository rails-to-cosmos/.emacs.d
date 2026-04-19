;;; make.el --- Makefile target completion and execution  -*- lexical-binding: t -*-

;;; Code:

(declare-function eshell-send-input "esh-mode")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(defvar eshell-buffer-name)

(defun make--send-command (buffer-name command)
  "Send COMMAND to BUFFER-NAME using vterm or fall back to eshell."
  (if (fboundp 'vterm)
      (condition-case err
          (progn
            (vterm buffer-name)
            (vterm-send-string command)
            (vterm-send-return))
        (error
         (message "Warning: vterm failed, falling back to eshell")
         (make--send-to-eshell buffer-name command)))
    (message "Warning: vterm is not available, falling back to eshell")
    (make--send-to-eshell buffer-name command)))

(defun make--send-to-eshell (buffer-name command)
  "Send COMMAND to BUFFER-NAME using eshell."
  (with-current-buffer (let ((eshell-buffer-name buffer-name)) (eshell))
    (insert command)
    (eshell-send-input)))

(cl-defun make-completing-read ()
  "Search and scan dominating Makefile - choose a make target - run it in eshell."
  (interactive)

  (let (output-buffer-name)
    (save-window-excursion
      (with-temp-buffer
        (when-let (default-directory (locate-dominating-file default-directory "Makefile"))
          (let* ((targets (cl-loop with makefile = (expand-file-name "Makefile" default-directory)
                                   initially (save-excursion (insert-file-contents makefile))
                                   while (re-search-forward "^\\([^#[:space:]\n][^:[:space:]]*\\):" nil t)
                                   for target = (match-string 1)
                                   unless (string= target ".PHONY")
                                   collect target into targets
                                   finally (return (sort targets #'string<))))
                 (target (completing-read "Make: " targets nil t))
                 (project-name (file-name-base (directory-file-name default-directory))))

            (setq output-buffer-name (format "*make-%s-%s*" project-name target))

            (when (buffer-live-p (get-buffer output-buffer-name))
              (kill-buffer output-buffer-name))

            (make--send-command output-buffer-name (format "make %s" target))))))

    (unless (equal (current-buffer) (get-buffer output-buffer-name))
      (switch-to-buffer-other-window output-buffer-name)
      (other-window -1))))

(provide 'make)

;;; make.el ends here
