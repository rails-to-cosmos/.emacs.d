(defun translate-text (sentence)
  "Translate SENTENCE."
  (interactive "sTranslate sentence: ")
  (with-current-buffer (get-buffer-create "*Translate*")
    (let ((inhibit-read-only t))
      (org-mode)
      (delete-region (point-min) (point-max))
      (cl-loop
         for (re . langs) in '(("[a-zA-Z]" . ("en" "ru"))
                               ("[a-zA-Z]" . ("czech" "ru"))
                               ("[а-яА-Я]" . ("ru" "en"))
                               ("[а-яА-Я]" . ("ru" "czech")))
         when (string-match re sentence)
         do (insert
             (format "* %s → %s\n" (car langs) (cadr langs))
             (shell-command-to-string (format "trans -s %s -t %s %s" (car langs) (cadr langs) sentence))
             "\n"))))
  (switch-to-buffer-other-window "*Translate*"))

(global-set-key (kbd "C-x y t t") #'translate-text)
