;;; table-view-csv.el --- View CSV/TSV files with table-view -*- lexical-binding: t; -*-

;; Author: Dmitry Akatov <akatovda@gmail.com>

;;; Commentary:
;; Render a CSV (or TSV / semicolon-separated) file as a `table-view':
;;
;;   M-x table-view-csv            view the current file buffer, else prompt
;;   M-x table-view-csv-find-file  prompt for a file and view it
;;   M-x table-view-csv-view-buffer  view the current buffer
;;
;; The delimiter is sniffed from the first line (comma / tab / semicolon).  The
;; first row is treated as a header (see `table-view-csv-header'); columns whose
;; every value parses as a number get numeric sorting and right alignment.  A CSV
;; at/above `table-view-native-threshold' rows loads `table-view-native' on demand
;; (when installed) and hands off to the Rust backend; small CSVs never load it.

;;; Code:

(require 'table-view)
(require 'cl-lib)
(require 'subr-x)

(defgroup table-view-csv nil
  "View CSV files with table-view." :group 'table-view :prefix "table-view-csv-")

(defcustom table-view-csv-header t
  "When non-nil, treat the first row as a header of column names." :type 'boolean)

;;; Parsing

(defun table-view-csv--sniff (text)
  "Guess the delimiter character of TEXT from its first line."
  (let ((line (car (split-string text "\n" t)))
        (best ?,) (bestn -1))
    (dolist (c '(?, ?\t ?\;) best)
      (let ((n (cl-count c (or line ""))))
        (when (> n bestn) (setq bestn n best c))))))

(defun table-view-csv--parse (text sep)
  "Parse TEXT as delimiter-SEP CSV into a list of records (lists of fields).
Fast-paths the quote-free common case; otherwise runs an RFC 4180 scanner that
honours quoted fields, \"\"-escaped quotes, and separators/newlines in quotes."
  (if (not (string-search "\"" text))
      (let ((re (regexp-quote (char-to-string sep))))
        (mapcar (lambda (l) (split-string l re))
                (split-string (string-trim-right text "[\r\n]+") "\r?\n")))
    (let ((i 0) (n (length text)) (records '()) (chars '()) (record '()) (q nil))
      (cl-flet ((field () (setq record (cons (apply #'string (nreverse chars)) record) chars '()))
                (row () (setq records (cons (nreverse record) records) record '())))
        (while (< i n)
          (let ((c (aref text i)))
            (cond
             (q (cond
                 ((and (eq c ?\") (< (1+ i) n) (eq (aref text (1+ i)) ?\"))
                  (push ?\" chars) (setq i (1+ i)))
                 ((eq c ?\") (setq q nil))
                 (t (push c chars))))
             ((eq c ?\") (setq q t))
             ((eq c sep) (field))
             ((eq c ?\r) nil)
             ((eq c ?\n) (field) (row))
             (t (push c chars))))
          (setq i (1+ i)))
        (when (or chars record) (field) (row))
        (nreverse records)))))

(defun table-view-csv--numeric-col-p (data i)
  "Non-nil when column I of DATA rows is present and entirely numeric."
  (let ((seen nil))
    (catch 'no
      (dolist (r data seen)
        (let ((v (and (nth i r) (string-trim (nth i r)))))
          (when (and v (not (string-empty-p v)))
            (setq seen t)
            (unless (string-match-p
                     "\\`[+-]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?\\'" v)
              (throw 'no nil))))))))

;;; Display

(defun table-view-csv--display (name text &optional buffer)
  "Render TEXT (CSV) as a table-view titled NAME, into BUFFER or a *csv:* buffer."
  (let* ((sep (table-view-csv--sniff text))
         (records (cl-remove-if (lambda (r) (equal r '(""))) (table-view-csv--parse text sep)))
         (_ (unless records (user-error "No CSV rows to display")))
         (header (and table-view-csv-header (car records)))
         (data (if table-view-csv-header (cdr records) records))
         (ncol (apply #'max 0 (mapcar #'length records)))
         (keys (cl-loop for i below ncol collect (format "c%d" i)))
         (columns
          (cl-loop for i below ncol collect
                   (append `((key . ,(nth i keys))
                             (header . ,(let ((h (and header (nth i header))))
                                          (if (and h (not (string-empty-p h))) h
                                            (format "col%d" (1+ i))))))
                           (when (table-view-csv--numeric-col-p data i)
                             '((type . "number") (align . "right"))))))
         (rows
          (cl-loop for r in data for n from 0 collect
                   `((id . ,(number-to-string n))
                     (cells . ,(cl-loop for i below ncol
                                        collect (cons (intern (nth i keys)) (or (nth i r) "")))))))
         (spec `((title . ,name) (columns . ,columns) (rows . ,rows))))
    ;; A large CSV pulls in the native backend (when installed) so
    ;; `table-view-display's seam can hand it to the Rust engine; small CSVs
    ;; never load it.
    (when (and (bound-and-true-p table-view-native-threshold)
               (>= (length rows) table-view-native-threshold))
      (require 'table-view-native nil t))
    (table-view-display (or buffer (format "*csv: %s*" name)) spec nil)))

;;; Auto-mode: view a visited CSV file in place

(defvar-local table-view-csv--file nil
  "The CSV file this table view was rendered from, or nil.")

(defun table-view-csv--render-file (buffer file text)
  "Render TEXT as a table into BUFFER, backed by FILE.
Detaches BUFFER from the file so the rendered table cannot be saved over it,
and installs revert (`g') and raw-visit (`C-c C-c') bindings.  Re-establishes
these each time, since `table-view-mode' clears buffer-local state on render."
  (table-view-csv--display (if file (file-name-nondirectory file) (buffer-name buffer))
                           text buffer)
  (with-current-buffer buffer
    (setq-local table-view-csv--file file)
    (setq buffer-file-name nil)          ; a view, not the file -- no save-over
    (set-buffer-modified-p nil)
    (setq-local revert-buffer-function
                (lambda (&rest _)
                  (when file
                    (table-view-csv--render-file
                     buffer file
                     (with-temp-buffer (insert-file-contents file) (buffer-string))))))
    (local-set-key (kbd "C-c C-c") #'table-view-csv-visit-raw)))

;;;###autoload
(defun table-view-csv-mode ()
  "Show the visited CSV/TSV file as a read-only table-view.
For `auto-mode-alist'.  The buffer is detached from the file (so the table
cannot be saved over it); `g' reloads and `C-c C-c' opens the raw text."
  (interactive)
  (table-view-csv--render-file (current-buffer) buffer-file-name
                               (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun table-view-csv-visit-raw ()
  "Open the raw text of the CSV file backing this table view."
  (interactive)
  (if table-view-csv--file
      (find-file table-view-csv--file)
    (user-error "This view has no backing file")))

;;;###autoload
(defun table-view-csv-view-buffer (&optional buffer)
  "View BUFFER (default the current buffer) as a CSV table-view."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (table-view-csv--display
     (buffer-name buf)
     (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun table-view-csv-find-file (file)
  "Read FILE and view it as a CSV table-view."
  (interactive "fCSV file: ")
  (table-view-csv--display
   (file-name-nondirectory file)
   (with-temp-buffer (insert-file-contents file) (buffer-string))))

;;;###autoload
(defun table-view-csv ()
  "View a CSV as a table-view: the visited file buffer, else a prompted file."
  (interactive)
  (if buffer-file-name
      (table-view-csv-view-buffer)
    (call-interactively #'table-view-csv-find-file)))

(provide 'table-view-csv)
;;; table-view-csv.el ends here
