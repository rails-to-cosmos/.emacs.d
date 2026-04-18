;;; parquet-mode.el --- Parquet file viewer with lazy pagination -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar parquet--script
  (expand-file-name "src/parquet-mode/parquet-info.py" user-emacs-directory)
  "Path to the parquet-info Python script (fallback when native module is unavailable).")

(defvar parquet--native-module
  (expand-file-name
   (format "src/parquet-mode/rust/target/release/libparquet_mod.%s"
           (if (eq system-type 'darwin) "dylib" "so"))
   user-emacs-directory)
  "Path to the compiled Rust/polars dynamic module.")

(defvar parquet--native-loaded nil
  "Non-nil if the native parquet-mod module has been successfully loaded.")

(defun parquet--try-load-native ()
  "Attempt to load the native module once. Return non-nil on success."
  (or parquet--native-loaded
      (setq parquet--native-loaded
            (and (fboundp 'module-load)
                 (file-exists-p parquet--native-module)
                 (ignore-errors
                   (module-load parquet--native-module)
                   t)))))

(defvar parquet-page-size 10
  "Number of rows to load per page.")

(defvar-local parquet--file nil
  "The parquet file being viewed.")

(defvar-local parquet--total-rows nil
  "Total number of rows in the parquet file.")

(defvar-local parquet--current-offset 0
  "Current page offset.")

(defvar-local parquet--info-text nil
  "Cached metadata/schema text for the file.")

(defvar parquet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map (kbd "C-S-n") #'parquet-next-page)
    (define-key map (kbd "C-S-p") #'parquet-prev-page)
    (define-key map "$" #'parquet-last-page)
    (define-key map "^" #'parquet-first-page)
    (define-key map "g" #'parquet-refresh)
    (define-key map "?" #'parquet-help)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `parquet-mode'.")

(define-derived-mode parquet-mode org-mode "Parquet"
  "Major mode for viewing Parquet files with lazy pagination."
  (setq buffer-read-only t))

(defun parquet--run-script (command file &rest args)
  "Execute COMMAND (\"info\" or \"page\") for FILE with ARGS. Return output text.
Prefers the native Rust/polars module when available; falls back to the
Python `parquet-info.py' script otherwise."
  (if (and (parquet--try-load-native)
           (fboundp (intern (format "parquet-mod-%s" command))))
      (apply (intern (format "parquet-mod-%s" command)) file args)
    (with-temp-buffer
      (let ((exit-code (apply #'call-process parquet--script nil t nil
                              command file (mapcar #'number-to-string args))))
        (unless (= exit-code 0)
          (error "parquet-info %s failed (exit %d): %s" command exit-code (buffer-string)))
        (buffer-string)))))

(defun parquet--align-tables ()
  "Align all org tables in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^|" nil t)
      (org-table-align)
      (goto-char (org-table-end)))))

(defun parquet--paginator-string ()
  "Return a paginator string showing current page position."
  (let* ((total (or parquet--total-rows 0))
         (page-num (1+ (/ parquet--current-offset parquet-page-size)))
         (total-pages (max 1 (ceiling total parquet-page-size)))
         (row-start (1+ parquet--current-offset))
         (row-end (min (+ parquet--current-offset parquet-page-size) total)))
    (format "Page %d/%d (rows %d–%d of %d)  [C-S-p ◀ prev | next ▶ C-S-n]  [^ first | last $]"
            page-num total-pages row-start row-end total)))

(defun parquet--render-page ()
  "Render the current page into the buffer."
  (let ((output (parquet--run-script "page" parquet--file
                                     parquet--current-offset parquet-page-size)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert parquet--info-text)
      (insert (parquet--paginator-string) "\n\n")
      (insert output "\n")
      (parquet--align-tables))
    (parquet--update-header)
    (goto-char (point-min))
    (when (re-search-forward "^\\* Data" nil t)
      (forward-line 1))))

(defun parquet--update-header ()
  "Update the header-line to show paging progress."
  (let* ((page-end (min (+ parquet--current-offset parquet-page-size)
                        (or parquet--total-rows 0)))
         (total (or parquet--total-rows 0)))
    (setq header-line-format
          (format " Parquet: %s — rows %d–%d of %d — C-S-n/C-S-p page, ? help"
                  (file-name-nondirectory parquet--file)
                  (1+ parquet--current-offset)
                  page-end
                  total))))

;;;###autoload
(defun parquet-next-page ()
  "Show the next page of parquet data."
  (interactive)
  (let ((new-offset (+ parquet--current-offset parquet-page-size)))
    (if (>= new-offset (or parquet--total-rows 0))
        (message "Already at last page")
      (setq parquet--current-offset new-offset)
      (parquet--render-page))))

;;;###autoload
(defun parquet-prev-page ()
  "Show the previous page of parquet data."
  (interactive)
  (if (<= parquet--current-offset 0)
      (message "Already at first page")
    (setq parquet--current-offset (max 0 (- parquet--current-offset parquet-page-size)))
    (parquet--render-page)))

(defun parquet-first-page ()
  "Jump to the first page."
  (interactive)
  (if (= parquet--current-offset 0)
      (message "Already at first page")
    (setq parquet--current-offset 0)
    (parquet--render-page)))

(defun parquet-last-page ()
  "Jump to the last page."
  (interactive)
  (let* ((total (or parquet--total-rows 0))
         (last-offset (* (/ (max 0 (1- total)) parquet-page-size) parquet-page-size)))
    (if (= parquet--current-offset last-offset)
        (message "Already at last page")
      (setq parquet--current-offset last-offset)
      (parquet--render-page))))

;;;###autoload
(defun parquet-refresh ()
  "Re-read metadata and refresh the current page."
  (interactive)
  (let* ((info (parquet--run-script "info" parquet--file))
         (total (with-temp-buffer
                  (insert info)
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+PROPERTY: rows \\([0-9]+\\)" nil t)
                    (string-to-number (match-string 1))))))
    (setq parquet--info-text info
          parquet--total-rows total))
  (parquet--render-page)
  (message "Refreshed"))

(defun parquet-help ()
  "Show parquet-mode keybindings."
  (interactive)
  (message "n/p: line  C-S-n/C-S-p: page  ^/$: first/last  g: refresh  q: quit"))

;;;###autoload
(defun parquet-open (file)
  "Open FILE as a parquet buffer with metadata and lazy data loading."
  (interactive "fParquet file: ")
  (let* ((file (expand-file-name file))
         (buf-name (format "*parquet:%s*" (file-name-nondirectory file)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let* ((info (parquet--run-script "info" file))
             (total (with-temp-buffer
                      (insert info)
                      (goto-char (point-min))
                      (when (re-search-forward "^#\\+PROPERTY: rows \\([0-9]+\\)" nil t)
                        (string-to-number (match-string 1))))))
        (parquet-mode)
        (setq parquet--file file
              parquet--current-offset 0
              parquet--info-text info
              parquet--total-rows total))
      (parquet--render-page))
    (pop-to-buffer buf)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.parquet\\'" . parquet--auto-open))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.parquet\\.gz\\'" . parquet--auto-open))

;; Also catch parquet files by magic bytes (PAR1 header)
(add-to-list 'magic-mode-alist '("PAR1" . parquet--auto-open))

(defun parquet--auto-open ()
  "Hook for `auto-mode-alist' to open parquet files."
  (let ((file buffer-file-name))
    (kill-buffer (current-buffer))
    (parquet-open file)))

(defun parquet--file-p (file)
  "Return non-nil if FILE is a parquet file."
  (string-match-p "\\.parquet\\(?:\\.gz\\)?\\'" file))

(defun parquet--dired-find-file-advice (orig-fn &rest args)
  "Intercept dired-find-file to open parquet files with parquet-open."
  (let ((file (dired-get-file-for-visit)))
    (if (parquet--file-p file)
        (parquet-open file)
      (apply orig-fn args))))

(advice-add 'dired-find-file :around #'parquet--dired-find-file-advice)

(provide 'parquet-mode)
;;; parquet-mode.el ends here
