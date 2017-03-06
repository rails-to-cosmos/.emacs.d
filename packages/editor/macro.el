(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun save-macro (name)
  "save a macro. Take a name as argument
        and save the last defined macro under
        this name at the end of your .emacs"
  (interactive "SName of the macro:")  ; ask for the name of the macro
  (kmacro-name-last-macro name)        ; use this name for the macro
  (find-file user-init-file)   ; open ~/.emacs or other user init file
  (goto-char (point-max))      ; go to the end of the .emacs
  (newline)                    ; insert a newline
  (insert-kbd-macro name)      ; copy the macro
  (newline)                    ; insert a newline
  (switch-to-buffer nil))

(provide 'macro)
