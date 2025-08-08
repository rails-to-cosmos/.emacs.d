(defun my-rc ()
  (interactive)
  (find-file "~/.zshrc"))

(global-set-key (kbd "C-x y r c") #'my-rc)

(provide 'init-helpers)
