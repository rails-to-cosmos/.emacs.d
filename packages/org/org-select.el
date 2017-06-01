;;; org-select.el --- Provide html-like select input functionallity based on org-links, ido and org-babel.
;;; Commentary:
;;; Code:

(org-add-link-type
 "select" 'org-select)

(defun mark-org-link-at-point ()
  "Select the link under cursor."
  (interactive)
  (let (org-link-pattern begginning-of-org-link end-of-org-link)
    (setq org-link-pattern "\\[\\[\\w+:.*\\]\\]")

    ;; Move at begginning of org-link
    (while (progn
             (if (not (looking-at org-link-pattern))
                 (search-backward "["))
             (not (looking-at org-link-pattern))))
    (setq begginning-of-org-link (point))

    ;; Mark org-link label
    (search-forward "][")
    (set-mark-command nil)
    (search-forward "]]")
    (backward-char 2)
    (setq end-of-org-link (point))
    (setq deactivate-mark nil)))

(defun org-select (fn)
  "Insert org-selector"
  (setq options (split-string (eval (read (concat "(org-sbe " fn ")"))) ", "))
  (mark-org-link-at-point)
  (let ((current-option (buffer-substring (mark) (point)))
        selected-value selected-index babel-result)
    (setq selected-value (ido-completing-read "Choose value: " options))
    (setq selected-index (position selected-value options :test #'equal))
    (setq babel-result (eval (read (concat "(org-sbe " fn " (index " (int-to-string selected-index) "))"))))
    (delete-region (mark) (point))
    (insert babel-result)
    (org-table-map-tables 'org-table-align)))

(provide 'org-select)
;;; org-select.el ends here
