(use-package table-view
  :ensure t)

(use-package org-glance
  :bind (("C-x j" . org-glance-transient)
         ;; :map org-mode-map
         ;; ("@" . org-glance:@)
         )
  :config (org-glance-init)
  :ensure t)

;; (require 'project)
;; (cl-defun my-jump-meta ()
;;   (interactive)
;;   (cl-loop with dir = default-directory
;;            with metafiles = '()
;;            while (and (not (null dir)) (null metafiles))
;;            do (progn
;;                 (setq metafiles (directory-files dir nil "\\`[^.].*\\.\\(org\\|nix\\)\\'"))
;;                 (if (null metafiles) (setq dir (f-parent dir))))
;;            finally do (pcase metafiles
;;                         ('() (message "No meta files found in current project"))
;;                         (`(,file) (find-file (f-join dir file)))
;;                         (files (let ((file (completing-read "Choose project meta file: " files)))
;;                                  (find-file (f-join dir file)))))))
;; (global-set-key (kbd "C-x y o") #'my-jump-meta)

(provide 'mijn-glance)
