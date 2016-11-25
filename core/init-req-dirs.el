;;; init-req-dirs.el --- my default directories
;;; Commentary:
;;; Code:

(defun init-req-dirs () "Initialize init-req-dirs with my configuration." (interactive))

(defun custom/ (filename dirname)
  "Get path to FILENAME in DIRNAME."
  (make-directory dirname t)
  (concat dirname filename))

(defun tmp/ (filename)
  "Get path to FILENAME in temp directory."
  (custom/ filename (concat user-emacs-directory "tmp/")))

(defun dropbox/ (filename)
  "Get path to FILENAME in dropbox directory."
  (custom/ filename "~/Dropbox/"))

(setq-default
 custom-file (tmp/ "custom.el"))

(provide 'init-req-dirs)
;;; init-req-dirs.el ends here
