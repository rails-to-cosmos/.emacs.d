;;; org-select.el --- Provide html-like select input functionallity based on org-links, ido and org-babel.
;;; Commentary:
;;; Code:
(require 'subr-x)

(org-add-link-type
 "select" 'org-select)

(defun org-select (handler)
  "Insert org-selector"
  (mark-org-link-at-point)
  (let ((val (ido-completing-read "Choose value:"
              (split-string
               (eval (read (concat "(org-sbe " handler " (action " (prin1-to-string "\"options\"") "))")))
              "\n"))))
    (delete-region (mark) (point))
    (insert
     (eval
      (read
       (concat
        "(org-sbe "
        handler
        " (action " (prin1-to-string "\"choose\"") ")"
        " (option " "(prin1-to-string val))"
        ")"))))
    (org-table-map-tables 'org-table-align)))

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

(provide 'org-select)
;;; org-select.el ends here
