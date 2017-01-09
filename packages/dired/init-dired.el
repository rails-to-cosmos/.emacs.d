;;; init-dired.el --- my settings for dired
;;; Commentary:
;;; Code:

(defun install-dired-x ()
  "Initialize dired with my configuration."
  (interactive)

  (use-package dired+
    :init (progn
            (after-load 'dired-x
              (setq-default dired-use-ls-dired nil)))
    :config (progn
              (diredp-toggle-find-file-reuse-dir 1))
    :ensure t))

(defun dired/sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before mark adding."
  (dired/sort))

(defun dired/hide-cursor ()
  "Hide cursor in dired buffer."
  (setq cursor-type nil))

(use-package dired-narrow
  :ensure t)

(use-package dired-rainbow
  :config (progn
            (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
            (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg"))
            (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
            (dired-rainbow-define-chmod executable-unix "#B3DE81" "-[rw-]+x.*"))
  :ensure t)

(defun dired/switch-or-jump ()
  "Quickly switch to dired buffer."
  (interactive)
  (progn (let ((dbufs  (cl-remove-if-not
                        (lambda (bf)
                          (with-current-buffer bf
                            (derived-mode-p 'dired-mode)))
                        (buffer-list))))
           (if dbufs
               (switch-to-buffer (car dbufs))
             (dired-jump)))))

(provide 'init-dired)
;;; init-dired.el ends here
