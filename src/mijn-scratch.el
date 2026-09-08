;;; mijn-scratch.el --- Scratch buffer setup -*- lexical-binding: t; -*-

(setq-default initial-major-mode 'emacs-lisp-mode)

(setq-default initial-scratch-message
              (s-join "\n\n" (--mapcat (list (concat ";; " it))
                                       '("Do stuff that you want to, don't rely on productivity to give you validation. Have heart"
                                         "I've always thought they were lighthouses..."))))

(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer) nil) t))

(add-hook 'kill-buffer-query-functions 'immortal-scratch)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(provide 'mijn-scratch)

;;; mijn-scratch.el ends here
