(defun spawn-shell (name &rest commands)
  "Invoke shell with commands"
  (interactive "MName of shell buffer to spawn: ")
  (pop-to-buffer (get-buffer-create name))
  (setq default-eshell-buffer-name (if (string= (boundp 'eshell-buffer-name) nil)
                                       "*eshell*"
                                     eshell-buffer-name))
  (setq eshell-buffer-name name)
  (eshell)
  (setq eshell-buffer-name default-eshell-buffer-name)
  (loop for command in commands
        do (insert (concat command "\n")))
  (eshell-send-input)
  (goto-char (point-max)))

(use-package eshell-prompt-extras
  :config
  (venv-initialize-eshell)
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-dakrone)
  (with-eval-after-load "esh-opt"
    (require 'virtualenvwrapper)
    (venv-initialize-eshell)
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-dakrone))
  :ensure t
  :ensure virtualenvwrapper)

(provide 'init-shell)
