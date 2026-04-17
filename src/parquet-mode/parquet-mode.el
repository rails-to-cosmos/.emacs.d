;;; parquet-mode.el --- Parquet file viewer with lazy pagination -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar parquet--script
  (expand-file-name "src/parquet-mode/parquet-info.py" user-emacs-directory)
  "Path to the parquet-info Python script.")

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

(defvar-local parquet--sort-column nil
  "Column name to sort by, or nil for natural order.")

(defvar-local parquet--sort-desc nil
  "Non-nil for descending sort.")

(defvar parquet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map (kbd "C-S-n") #'parquet-next-page)
    (define-key map (kbd "C-S-p") #'parquet-prev-page)
    (define-key map ">" #'parquet-last-page)
    (define-key map "<" #'parquet-first-page)
    (define-key map "^" #'parquet-sort-by-column)
    (define-key map "g" #'parquet-refresh)
    (define-key map "?" #'parquet-help)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `parquet-mode'.")

(define-derived-mode parquet-mode org-mode "Parquet"
  "Major mode for viewing Parquet files with lazy pagination."
  (setq buffer-read-only t))

(defun parquet--run-script (command file &rest args)
  "Run the parquet-info script with COMMAND, FILE, and ARGS. Return stdout.
ARGS are converted to strings. Use strings directly for keyword arguments."
  (with-temp-buffer
    (let ((str-args (mapcar (lambda (a) (if (stringp a) a (number-to-string a))) args)))
      (let ((exit-code (apply #'call-process parquet--script nil t nil
                              command file str-args)))
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
         (row-end (min (+ parquet--current-offset parquet-page-size) total))
         (sort-info (if parquet--sort-column
                        (format "  sorted by: %s %s"
                                parquet--sort-column
                                (if parquet--sort-desc "▼" "▲"))
                      "")))
    (format "Page %d/%d (rows %d–%d of %d)%s  [C-S-p ◀ | ▶ C-S-n]  [< first | last >]  [^ sort]"
            page-num total-pages row-start row-end total sort-info)))

(defun parquet--render-page ()
  "Render the current page into the buffer."
  (let ((output (apply #'parquet--run-script "page" parquet--file
                       parquet--current-offset parquet-page-size
                       (append
                        (when parquet--sort-column
                          (list "--sort_by" parquet--sort-column))
                        (when parquet--sort-desc
                          (list "--sort_desc"))))))
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

(defun parquet--column-at-point ()
  "Return the column name at point if inside an org table in the Data section."
  (when (and (org-at-table-p)
             (save-excursion
               (re-search-backward "^\\* Data" nil t)))
    (let ((col-idx (org-table-current-column)))
      (save-excursion
        (goto-char (org-table-begin))
        (org-table-goto-column col-idx)
        (when (looking-at "\\s-*\\([^ |]+\\)")
          (string-trim (match-string 1)))))))

(defun parquet-sort-by-column ()
  "Sort the data by the column at point. Toggle asc/desc on repeated press."
  (interactive)
  (let ((col (parquet--column-at-point)))
    (unless col
      (user-error "Not on a data table column"))
    (if (equal col parquet--sort-column)
        (if parquet--sort-desc
            ;; Already desc — clear sort
            (setq parquet--sort-column nil
                  parquet--sort-desc nil)
          ;; Was asc — switch to desc
          (setq parquet--sort-desc t))
      ;; New column — sort ascending
      (setq parquet--sort-column col
            parquet--sort-desc nil))
    (setq parquet--current-offset 0)
    (parquet--render-page)
    (if parquet--sort-column
        (message "Sort: %s %s" parquet--sort-column (if parquet--sort-desc "desc" "asc"))
      (message "Sort cleared"))))

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
  (message "n/p: line  C-S-n/C-S-p: page  </>: first/last  ^: sort column  g: refresh  q: quit"))

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
