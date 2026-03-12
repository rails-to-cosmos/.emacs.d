(require 'ol)

(cl-defun akatovda:browse-directory-in-eshell (directory &rest args)
  (let ((default-directory directory))
    (eshell '(4))))

(cl-defun akatovda:store-current-directory ()
  (when (memq major-mode '(eshell-mode))
    (org-link-store-props
     :type "eshell"
     :link (concat "eshell:" default-directory)
     :description "Directory for eshell")))

(cl-defun akatovda:complete-directory (&optional arg)
  (concat "eshell:" (read-directory-name "Choose directory: ")))

(org-link-set-parameters
 "eshell"
 :follow #'akatovda:browse-directory-in-eshell
 :store #'akatovda:store-current-directory
 :complete #'akatovda:complete-directory)

(provide-me)
