;;; init-dired.el --- my settings for dired
;;
;; Filename: init-dired.el
;; Description: my settings for dired
;; Author: Dmitry Akatov
;; Created: <2016-11-24 Thu 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d/core/init-dired.el
;; Keywords: Emacs 24.5
;; Compatibility: emacs >= 24.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun init-dired ()
  "Initialize dired with my configuration."
  (interactive)
  (use-package dired+
    :config (diredp-toggle-find-file-reuse-dir t)
    :ensure t))

(use-package dired-narrow
  :ensure t)

(use-package dired-list)

(use-package dired-rainbow
  :config
  (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
  (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg"))
  (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
  (dired-rainbow-define-chmod executable-unix "#B3DE81" "-[rw-]+x.*")
  :ensure t)

(defun dired/sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defun dired/hide-cursor ()
  "Hide cursor in dired buffer."
  (setq cursor-type nil))

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
