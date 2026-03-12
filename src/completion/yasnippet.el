(require 'yasnippet)

(defvar-local yas--expandable-keys-overlay nil
  "Basic overlay for possible yassnipets.")

(defun yas-show-expand-keys ()
  "Put overlay on text which is an expandable snippet key.
This function is intended to be added to `post-command-hook'."
  (let ((keys-at-point (and yas-minor-mode (yas--templates-for-key-at-point)))
        (have-overlay (overlayp (buffer-local-value 'yas--expandable-keys-overlay (current-buffer)))))
    (if keys-at-point
        (let ((beg (nth 1 keys-at-point))
              (end (nth 2 keys-at-point)))
          (if have-overlay
              (move-overlay yas--expandable-keys-overlay beg end)
            (setq-local yas--expandable-keys-overlay
                        (make-overlay beg end)))
          (overlay-put yas--expandable-keys-overlay 'face '(:box t)))
      (when have-overlay
        (delete-overlay yas--expandable-keys-overlay)))))

(setq yas-snippet-dirs (list (f-join user-emacs-directory "snippets")))

(add-hook 'prog-mode-hook #'yas-minor-mode)
;; (add-hook 'post-command-hook #'yas-show-expand-keys)

(yas-reload-all)
(yas-recompile-all)

(define-key prog-mode-map (kbd "C-x i") #'company-yasnippet)
