;;; parquet-mode.el --- Parquet file viewer with lazy pagination -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar parquet--script
  (expand-file-name "src/parquet-mode/parquet-info.py" user-emacs-directory)
  "Path to the parquet-info Python script.")

(defvar parquet-page-size 50
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
  "Run the parquet-info script with COMMAND, FILE, and ARGS. Return stdout."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process parquet--script nil t nil
                            command file (mapcar #'number-to-string args))))
      (unless (= exit-code 0)
        (error "parquet-info %s failed (exit %d): %s" command exit-code (buffer-string)))
      (buffer-string))))

(defun parquet--align-tables ()
  "Align all org tables in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^|" nil t)
      (org-table-align)
      (goto-char (org-table-end)))))

(defun parquet--render-page ()
  "Render the current page into the buffer."
  (let ((output (parquet--run-script "page" parquet--file
                                     parquet--current-offset parquet-page-size)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert parquet--info-text)
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
  (let ((info (parquet--run-script "info" parquet--file)))
    (setq parquet--info-text info)
    (with-temp-buffer
      (insert info)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+PROPERTY: rows \\([0-9]+\\)" nil t)
        (setq parquet--total-rows (string-to-number (match-string 1))))))
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
      (let ((info (parquet--run-script "info" file)))
        (parquet-mode)
        (setq parquet--file file)
        (setq parquet--current-offset 0)
        (setq parquet--info-text info)
        (with-temp-buffer
          (insert info)
          (goto-char (point-min))
          (when (re-search-forward "^#\\+PROPERTY: rows \\([0-9]+\\)" nil t)
            (setq parquet--total-rows (string-to-number (match-string 1))))))
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

(provide 'parquet-mode)
;;; parquet-mode.el ends here
