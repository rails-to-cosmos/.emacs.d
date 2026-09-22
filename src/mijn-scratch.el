;;; mijn-scratch.el --- Scratch buffer setup -*- lexical-binding: t; -*-

(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer) nil) t))

(let ((scratch-thoughts
       '("I've always thought they were lighthouses..."
         "Do stuff that you want to"
         "Don't rely on productivity to give you validation"
         "Have heart")))
  
  (add-hook 'kill-buffer-query-functions 'immortal-scratch)
  
  (setq initial-major-mode 'emacs-lisp-mode
        initial-scratch-message (s-join "\n\n" (--mapcat (list (concat ";; " it)) scratch-thoughts))
        kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)))

(provide 'mijn-scratch)

;;; mijn-scratch.el ends here
