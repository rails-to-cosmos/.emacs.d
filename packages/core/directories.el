;;; directories.el --- my default directories
;;; Commentary:
;;; Code:

(defun custom/ (filename dirname)
  "Get path to FILENAME in DIRNAME."
  (make-directory dirname t)
  (concat dirname filename))

(defun tmp/ (filename)
  "Get path to FILENAME in temp directory."
  (custom/ filename (concat user-emacs-directory "tmp/")))

(defun dropbox/ (filename)
  "Get path to FILENAME in dropbox directory."
  (custom/ filename "~/Dropbox/Emacs/"))

(setq-default
 custom-file (tmp/ "custom.el"))

(provide 'directories)
;;; directories.el ends here
