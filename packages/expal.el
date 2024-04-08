(require 'load-relative)
(require 'highlight)

(defgroup expal nil
  "Highlighting."
  :prefix "expal-" :group 'editing :group 'convenience :group 'wp :group 'faces
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "akatovda" "@" "yandex" ".com?subject=\
expal.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  ;; :link '(url-link :tag "Download" "https://github.com/rails-to-cosmos/expal.el")
  ;; :link '(url-link :tag "Description" "https://github.com/rails-to-cosmos/expal.el")
  :link '(emacs-commentary-link :tag "Commentary" "expal"))

(defface expal-block-success-face
    '((((background dark)) (:background "dark green"))
      (t (:background "honeydew")))
  "*Face used to highlight evaluated paragraph."
  :group 'expal :group 'faces)

(defface expal-block-failed-face
    '((((background dark)) (:background "dark red"))
      (t (:background "misty rose")))
  "*Face used to highlight evaluated paragraph."
  :group 'expal :group 'faces)

(defface expal-block-hover-face
    '((((background dark)) (:background "dim grey"))
      (t (:background "white smoke")))
  "*Face used to highlight evaluated paragraph."
  :group 'expal :group 'faces)

(set-face-extend 'expal-block-success-face t)
(set-face-extend 'expal-block-failed-face t)
(set-face-extend 'expal-block-hover-face t)

(cl-defun expal:earliest-unclosed-bracket (open close)
  (let ((opened 0)
        (result (point)))
    (save-excursion
      (while (> (point) (point-min))
        (while (and (> (point) (point-min)) (<= opened 0))
          (backward-char)
          (cond ((looking-at open) (cl-incf opened))
                ((looking-at close) (cl-decf opened))))
        (when (> opened 0)
          (setq result (point)
                opened 0))))
    (goto-char result)
    (beginning-of-line)
    (point)))

(cl-defun expal:bounds ()
  (save-excursion
    (beginning-of-line)
    (let ((start (min (expal:earliest-unclosed-bracket "{" "}")
                      (expal:earliest-unclosed-bracket "\\[" "\\]")
                      (expal:earliest-unclosed-bracket "(" ")")))
          end
          (opened-round-brackets 0)
          (opened-curly-brackets 0)
          (opened-square-brackets 0))
      (while (or (not (eolp))
                 (> opened-round-brackets 0)
                 (> opened-curly-brackets 0)
                 (> opened-square-brackets 0))
        (cond ((looking-at "(") (cl-incf opened-round-brackets))
              ((looking-at ")") (cl-decf opened-round-brackets))
              ((looking-at "{") (cl-incf opened-curly-brackets))
              ((looking-at "}") (cl-decf opened-curly-brackets))
              ((looking-at "\\[") (cl-incf opened-square-brackets))
              ((looking-at "\\]") (cl-decf opened-square-brackets)))
        (forward-char))
      (setq end (+ (point) 1))
      (cons start end))))

(cl-defun expal:unhighlight ()
  (interactive)
  (let* ((bounds (expal:bounds))
         (beg (car bounds))
         (end (cdr bounds)))
    (hlt-unhighlight-region beg end)))

(cl-defun expal:highlight (&optional (face 'region))
  (interactive)
  (let* ((bounds (expal:bounds))
         (beg (car bounds))
         (end (cdr bounds)))
    (pulse-momentary-highlight-region beg end face)))

(cl-defun expal:unhighlight-hook ()
  (when (and (string= major-mode "emacs-lisp-mode")
             (eq this-command 'self-insert-command)
             (s-starts-with? "expal" (symbol-name (face-at-point))))
    (expal:unhighlight)))

(add-hook 'post-command-hook 'expal:unhighlight-hook)

(defun expal:mark ()
  (interactive)
  (let* ((bounds (expal:bounds))
         (beg (car bounds))
         (end (cdr bounds)))
    (goto-char beg)
    (set-mark end)))

(provide 'expal)
