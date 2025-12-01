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
                 (target (completing-read "Choose make target: " targets nil t))
                 (project-name (file-name-base (directory-file-name default-directory))))

            (setq output-buffer-name (format "*make-%s-%s*" project-name target))

            (when (buffer-live-p (get-buffer output-buffer-name))
              (kill-buffer output-buffer-name))

            (with-current-buffer (let ((eshell-buffer-name output-buffer-name)) (eshell))
              (insert (format "make %s" target))
              (eshell-send-input))))))

    (unless (equal (current-buffer) (get-buffer output-buffer-name))
      (switch-to-buffer-other-window output-buffer-name)
      (other-window -1))))

(provide 'make)
