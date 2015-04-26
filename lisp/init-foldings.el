(use-package hideshow
  :ensure t)

(global-set-key (kbd "C-+") 'hs-toggle-hiding)

(load-library "hideshow")

(add-hook 'php-mode-hook 'hs-minor-mode)

(defun hs-save-state (filename)
  (let ((overlays nil))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'hs)
        (setq overlays (cons (overlay-start ov) overlays))))
    (if overlays
        (with-temp-file filename
          (prin1 overlays (current-buffer)))
      (when (file-exists-p filename)
        (delete-file filename)))))

(defun hs-restore-state (filename)
  (when (file-exists-p filename)
    (let ((overlays
           (read (with-temp-buffer
                   (insert-file-contents-literally filename)
                   (buffer-string)))))
      (when (listp overlays)
        (save-excursion
          (mapc '(lambda (pos)
                   (goto-char pos)
                   (hs-hide-block))
                overlays))))))

(setq hs-hide-hook (lambda ()
                     (hs-save-state (concat (buffer-name) ".hs"))))

(setq hs-minor-mode-hook
      '(lambda ()
         (if hs-minor-mode
             (hs-restore-state (concat (buffer-name) ".hs")))))

(setq hs-hide-hook (lambda () (set-buffer-modified-p t)))
(setq hs-show-hook (lambda () (set-buffer-modified-p t)))

(add-hook 'after-save-hook
          (lambda ()
            (if hs-minor-mode
                (hs-save-state (concat (buffer-name) ".hs")))))

(setq hs-minor-mode-hook
      '(lambda ()
         (if hs-minor-mode
             (hs-restore-state (concat (buffer-name) ".hs")))))

(provide 'init-foldings)
