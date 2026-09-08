;;; mijn-fonts.el --- Pick an installed default font, warn if none  -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets the default frame font from the first available candidate in
;; `mijn-fonts-candidates'.  A missing font is a warning, never an error,
;; so a machine without the preferred fonts still starts cleanly.

;;; Code:

(defvar mijn-fonts-candidates
  '("JetBrains Mono NL-11"
    "JetBrains Mono-11"
    "DejaVu Sans Mono-11"
    "Liberation Mono-11"
    "Menlo-11"
    "Consolas-11")
  "Font specs to try, in order.  First installed one wins.")

(defun mijn-fonts-family (spec)
  "Return the family portion of font SPEC \"Family-Size\"."
  (car (split-string spec "-[0-9]" t)))

(defun mijn-fonts-apply ()
  "Set the default frame font to the first installed candidate.
Emit a warning instead of an error when none is available."
  (let ((chosen (seq-find (lambda (spec)
                            (find-font (font-spec :name (mijn-fonts-family spec))))
                          mijn-fonts-candidates)))
    (if chosen
        (set-frame-font chosen t t)
      (display-warning 'mijn-fonts
                       (format "None of the preferred fonts installed: %s. Using Emacs default."
                               (string-join mijn-fonts-candidates ", "))
                       :warning))))

(mijn-fonts-apply)

(provide 'mijn-fonts)
;;; mijn-fonts.el ends here
