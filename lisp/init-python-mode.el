(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  ;; (newline-and-indent)
  (insert "import pdb; pdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))

;; (defun python-interactive ()
;;   "Enter the interactive Python environment"
;;   (interactive)
;;   (progn
;;     (insert "!import code; code.interact(local=vars())")
;;     (move-end-of-line 1)
;;     (comint-send-input)))

;; (global-set-key (kbd "C-c i") 'python-interactive)

(define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)

(provide 'init-python-mode)
