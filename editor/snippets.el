(use-package yasnippet
  :config (progn
            ;; Completing point by some yasnippet key
            (defun yas-ido-expand ()
              "Lets you select (and expand) a yasnippet key"
              (interactive)
              (let ((original-point (point)))
                (while (and
                        (not (= (point) (point-min) ))
                        (not
                         (string-match "[[:space:]\n]" (char-to-string (char-before)))))
                  (backward-word 1))
                (let* ((init-word (point))
                       (word (buffer-substring init-word original-point))
                       (list (yas-active-keys)))
                  (goto-char original-point)
                  (let ((key (remove-if-not
                              (lambda (s) (string-match (concat "^" word) s)) list)))
                    (if (= (length key) 1)
                        (setq key (pop key))
                      (setq key (ido-completing-read "key: " list nil nil word)))
                    (delete-char (- init-word original-point))
                    (insert key)
                    (yas-expand)))))

            (yas-global-mode t))
  :ensure t)

(provide 'snippets)
