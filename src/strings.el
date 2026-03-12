(defun string-contains-p (substring string)
  "Return t if STRING contains SUBSTRING, otherwise return nil.
The search is case-sensitive."
  ;; string-match returns the starting index (a number) if found, or nil.
  ;; We convert the number to the boolean t for clarity.
  (not (null (string-match (regexp-quote substring) string))))

(provide 'strings)
