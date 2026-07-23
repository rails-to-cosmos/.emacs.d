(defun my-rc ()
  (interactive)
  (find-file "~/.zshrc"))

(defun my-copy-file-path ()
  "Copy the full path of the current buffer's file to the kill ring.
For dired buffers, copies `default-directory'."
  (interactive)
  (let ((path (or (buffer-file-name)
                  (and (derived-mode-p 'dired-mode) default-directory))))
    (if (not path)
        (user-error "Buffer is not visiting a file")
      (let ((p (expand-file-name path)))
        (kill-new p)
        (message "Copied: %s" p)))))

(global-set-key (kbd "C-x y r c") #'my-rc)
(global-set-key (kbd "C-x y r p") #'my-copy-file-path)

(provide 'mijn-helpers)
