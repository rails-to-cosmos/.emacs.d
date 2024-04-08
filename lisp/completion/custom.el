(defmacro define-completion (name string &rest body)
  (declare (doc-string 3) (indent 2))
  `(prog1
       (defun ,name nil
         ,(when (stringp (car body)) (car body))
         (interactive)
         (let ((point (point)))
           (when (condition-case nil
                     (when (save-excursion
                             (search-backward
                              ,string (- (point) (length ,string)) t))
                       ,@body)
                   (quit nil))
             (save-excursion
               (goto-char (- point (length ,string)))
               (delete-char (+ 0 (length ,string)))))))
     (add-hook 'post-self-insert-hook ',name)))

(define-completion org-complete-structure "<"
  "Complete org-structure template alist."
  (when (and (string= major-mode "org-mode")
             (not (org-in-src-block-p))
             (looking-back "^<" 1))
    (let* ((choice (org-completing-read "Template: " org-structure-template-alist))
           (item (cdr (assoc choice org-structure-template-alist))))
      (org-insert-structure-template item)
      (insert "\n")
      (previous-line)
      (previous-line)
      (delete-backward-char 1)
      (forward-line)
      t)))

(define-completion org-complete-rarrow "->"
  (when (and (string= major-mode "org-mode")
             (not (org-in-src-block-p)))
    (insert "→")
    t))
