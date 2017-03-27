;;; init-bindings.el --- my emacs bindings
;;; Commentary:
;;; Code:

(dolist (key '("\C-l" "\C-t" "\C-xi" "\C-cC-b" "\M-p"))
  (global-unset-key key))

(use-package key-combo
  :config (progn
            (global-key-combo-mode t)

            (key-combo-define-global (kbd "C-a") '(back-to-indentation
                                                   move-beginning-of-line
                                                   beginning-of-buffer
                                                   key-combo-return))
            (key-combo-define-global (kbd "C-e") '(move-end-of-line
                                                   end-of-buffer
                                                   key-combo-return)))
  :ensure t)

(defmacro define-context-key (keymap key dispatch)
  "Define in KEYMAP KEY to execute according to DISPATCH.

DISPATCH is a form that is evaluated and should return the
command to be executed.

If DISPATCH returns nil, then the command normally bound to KEY
will be executed."
  `(define-key ,keymap ,key
     `(menu-item "context-key" ignore
		 :filter ,(lambda (&optional ignored)
			    ,dispatch))))

(provide 'init-bindings)
;;; init-bindings.el ends here
