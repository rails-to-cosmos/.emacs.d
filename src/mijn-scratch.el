;;; mijn-scratch.el --- Scratch buffer setup -*- lexical-binding: t; -*-

(setq-default initial-major-mode 'fundamental-mode)

(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer) nil) t))

(add-hook 'kill-buffer-query-functions 'immortal-scratch)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(provide 'mijn-scratch)

;;; mijn-scratch.el ends here
