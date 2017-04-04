(require 'ox)
(defun gk-emphasis-markup (text backend info)
  (let ((formats (assoc backend gk-org-export-emphasis)))
    (when formats
      (let ((start 0) (result ""))
        (while (string-match gk-org-emph-re text start)
          (setq result
                (concat result
                        (substring text start (match-beginning 2))
                        (format (cadr (assoc (match-string 3 text) formats))
                                (match-string 4 text))))
          (setq start (- (match-end 0) 1)))
        (concat result
                (if
                    (substring text start (length text))
                    ""))))))
  (add-to-list 'org-export-filter-plain-text-functions 'gk-emphasis-markup)

(setq gk-org-export-emphasis
      '((html . (("?" "<span class=\"org-question\">%s</span>")
                 ("!" "<span class=\"org-exclamation\">%s</span>")
                 ("#" "")))
        (latex . (("?" "\\mycheck{%s}")
                  ("!" "\\myexcl{%s}")
                  ("#" "")))))

(setq gk-org-emphasis-alist
      (quote (
              ("×" (:foreground "#F34A4A"))
              ("!" (:foreground "#B3DE81"))
              ("?" (:foreground "#FFDB45"))
              ("#" font-lock-comment-face))))

(setq gk-org-emph-re
  (let* ((e org-emphasis-regexp-components)
         (pre (car e))
         (post (nth 1 e))
         (border (nth 2 e))
         (body (nth 3 e))
         (nl (nth 4 e))
         (body1 (concat body "*?"))
         (markers (mapconcat 'car gk-org-emphasis-alist ""))
         (vmarkers (mapconcat
                    (lambda (x) (if (eq (nth 4 x) 'verbatim) (car x) ""))
                    gk-org-emphasis-alist "")))
    ;; make sure special characters appear at the right position in the class
    (if (string-match "\\^" markers)
        (setq markers (concat (replace-match "" t t markers) "^")))
    (if (string-match "-" markers)
        (setq markers (concat (replace-match "" t t markers) "-")))
    (if (string-match "\\^" vmarkers)
        (setq vmarkers (concat (replace-match "" t t vmarkers) "^")))
    (if (string-match "-" vmarkers)
        (setq vmarkers (concat (replace-match "" t t vmarkers) "-")))
    (if (> nl 0)
        (setq body1 (concat body1 "\\(?:\n" body "*?\\)\\{0,"
                            (int-to-string nl) "\\}")))

    ;; Make the regexp
    (concat "\\([" pre "]\\|^\\)"
            "\\("
            "\\([" markers "]\\)"
            "\\("
            "[^" border "]\\|"
            "[^" border "]"
            body1
            "[^" border "]"
            "\\)"
            "\\3\\)"
            "\\([" post "]\\|$\\)")))

(defun gk-org-do-emphasis-faces (limit)
  "Run through the buffer and add overlays to emphasized strings."
  (let (rtn a)
    (while (and (not rtn) (re-search-forward gk-org-emph-re limit t))
      (if (not (= (char-after (match-beginning 3))
                  (char-after (match-beginning 4))))
          (progn
            (setq rtn t)
            (setq a (assoc (match-string 3) gk-org-emphasis-alist))
            (font-lock-prepend-text-property (match-beginning 2) (match-end 2)
                                             'face
                                             (nth 1 a))
            (and (nth 4 a)
                 (org-remove-flyspell-overlays-in
                  (match-beginning 0) (match-end 0)))
            (add-text-properties (match-beginning 2) (match-end 2)
                                 '(font-lock-multiline t org-emphasis t))
            (when org-hide-emphasis-markers
              (add-text-properties (match-end 4) (match-beginning 5)
                                   '(invisible org-link))
              (add-text-properties (match-beginning 3) (match-end 3)
                                   '(invisible org-link)))))
      (backward-char 1))
    rtn))

(font-lock-add-keywords
 'org-mode '(gk-org-do-emphasis-faces))
