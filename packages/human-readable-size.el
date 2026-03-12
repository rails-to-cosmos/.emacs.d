(provide 'human-readable-size)

(defconst iec-prefixes ["" "Ki" "Mi" "Gi"])

(defun human-readable-size (size)
  "Return SIZE as human-readable string, using IEC prefixes."
  (let* ((order (1- (max 1 (ceiling (log (max size 1) 1024)))))
	 (prefix (elt iec-prefixes (min order (length iec-prefixes))))
	 (size-in-unit (/ size (expt 1024.0 order)))
	 (precision
	  (max 3 (+ 2 (floor (log (max size-in-unit 1) 10)))))
	 (size-str
	  (format (format "%%.%dg%%sB" precision)
		  size-in-unit prefix)))
    size-str))
