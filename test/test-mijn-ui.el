;;; test-mijn-ui.el --- Theme behavior without a desktop bar -*- lexical-binding: t; -*-

(require 'ert)

(defvar mijn-theme-variant-file)
(defvar mijn-theme-sync-script)

(let ((source (expand-file-name "../src/mijn-ui.el"
                                (file-name-directory (or load-file-name buffer-file-name)))))
  (dolist (name '(mijn-theme-variant mijn-write-theme-variant
                  mijn-sync-emacs-theme xmobar-toggle-theme))
    (with-temp-buffer
      (insert-file-contents source)
      (goto-char (point-min))
      (re-search-forward (format "^(defun %s[ 	\n(]" name))
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))))

(ert-deftest mijn-ui-theme-toggle-without-xmonad ()
  "The theme toggle persists both variants without an xmonad script."
  (let* ((dir (make-temp-file "mijn-theme-" t))
         (mijn-theme-variant-file (expand-file-name "xmobar/theme-variant" dir))
         (mijn-theme-sync-script (expand-file-name "missing-theme-sync.sh" dir))
         (applied nil))
    (unwind-protect
        (cl-letf (((symbol-function 'mijn-apply-emacs-theme)
                   (lambda (variant) (push variant applied)))
                  ((symbol-function 'mijn-current-emacs-variant)
                   (lambda () (car applied)))
                  ((symbol-function 'start-process)
                   (lambda (&rest _) (error "xmonad script must not run"))))
          (xmobar-toggle-theme)
          (mijn-sync-emacs-theme)
          (should (eq (mijn-theme-variant) 'light))
          (should (equal (car applied) 'light))
          (xmobar-toggle-theme)
          (mijn-sync-emacs-theme)
          (should (eq (mijn-theme-variant) 'dark))
          (should (equal applied '(dark light))))
      (delete-directory dir t))))

(provide 'test-mijn-ui)
