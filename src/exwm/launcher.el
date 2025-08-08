(defun my-app-launcher ()
  (interactive)
  (when-let (app (completing-read "App: "
                                  (append (mapcar (lambda (c)
                                                    (let* ((name (caar c))
                                                           (cats (cdar c))
                                                           (str (concat name cats)))
                                                      (put-text-property 0 (length str) 'display (concat "d: " name) str)
                                                      (cons str (cdr c))))
                                                  (my-desktop-apps))

                                          (mapcar (lambda (s)
                                                    (put-text-property 0 (length s) 'display (concat "x: " s) s) s)
                                                  (my-executables-in-path)))))
    (if (consp app)
        (counsel-linux-app-action-default app)
      (start-process-shell-command app nil app))))

(defun my-executables-in-path ()
  (delete-dups
   (mapcar #'f-filename
           (-filter #'f-executable-p
                    (-flatten
                     (mapcar (lambda (dir)
                               (when (file-exists-p dir)
                                 (directory-files dir t directory-files-no-dot-files-regexp)))
                             (split-string (getenv "PATH") ":")))))))

(defun my-desktop-apps ()
  (mapcar (lambda (c)
            (cons (with-temp-buffer
                    (insert-file-contents (cdr c))
                    (cons
                     (progn (re-search-forward "^Name *= *\\(.+\\)$")
                            (match-string 1))
                     (progn (goto-char (point-min))
                            (re-search-forward "^Categories *= *\\(.+\\)$" nil t)
                            (match-string 1))))
                  (car c)))
          (counsel-linux-apps-list-desktop-files)))
