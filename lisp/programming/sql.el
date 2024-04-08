(add-to-list 'ob-languages '(sql . t))
(org-babel-do-load-languages 'org-babel-load-languages ob-languages)

(use-package sql-indent)

(defun org-tweaks--insert-org-mode-header ()
  (goto-char (point-min))
  (insert "#    -*- mode: org -*-\n\n"))

(defun org-tweaks--current-buffer-to-org-table ()
  (org-tweaks--insert-org-mode-header)
  (while (not (eobp))
    (let ((bol (save-excursion (beginning-of-line) (point)))
          (eol (save-excursion (forward-line +2) (end-of-line) (point))))
      (org-table-convert-region bol eol nil)
      (forward-line +3))))

(defun org-babel-execute:sql (body params)
  "Execute a block of Sql code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((connection (or (cdr (assq :conn params)) "default"))
         (result-params (or (cdr (assq :result-params params))
                            (getenv (concat (upcase connection) "_RESULT_PARAMS"))
                            (error "Unable to find env %s" (concat (upcase connection) "_RESULT_PARAMS"))))
         (cmdline (or (cdr (assq :cmdline params))
                      (getenv (concat (upcase connection) "_CMDLINE"))
                      ;; (error "Unable to find env %s" (concat (upcase connection) "_CMDLINE"))
                      ))
         (dbhost (or (cdr (assq :dbhost params))
                     (getenv (concat (upcase connection) "_DB_HOST"))
                     (error "Unable to find env %s" (concat (upcase connection) "_DB_HOST"))))
         (dbport (or (cdr (assq :dbport params))
                     (string-to-number (getenv (concat (upcase connection) "_DB_PORT")))
                     (error "Unable to find env %s" (concat (upcase connection) "_DB_PORT"))))
         (dbuser (or (cdr (assq :dbuser params))
                     (getenv (concat (upcase connection) "_DB_USER"))
                     (error "Unable to find env %s" (concat (upcase connection) "_DB_USER"))))
         (dbpassword (or (cdr (assq :dbpassword params))
                         (getenv (concat (upcase connection) "_DB_PASSWORD"))
                         (error "Unable to find env %s" (concat (upcase connection) "_DB_PASSWORD"))))
         (database (or (cdr (assq :database params))
                       (getenv (concat (upcase connection) "_DB_NAME"))
                       (error "Unable to find env %s" (concat (upcase connection) "_DB_NAME"))))
         (engine (or (cdr (assq :engine params))
                     (getenv (concat (upcase connection) "_ENGINE"))
                     (error "Unable to find env %s" (concat (upcase connection) "_ENGINE"))))
         (colnames-p (not (equal "no" (cdr (assq :colnames params)))))
         (in-file (org-babel-temp-file "sql-in-"))
         (out-file (or (cdr (assq :out-file params))
                       (org-babel-temp-file "sql-out-")))
	 (header-delim "")
         (command (pcase (intern engine)
                    (`dbi (format "dbish --batch %s < %s | sed '%s' > %s"
				  (or cmdline "")
				  (org-babel-process-file-name in-file)
				  "/^+/d;s/^|//;s/(NULL)/ /g;$d"
				  (org-babel-process-file-name out-file)))
                    (`monetdb (format "mclient -f tab %s < %s > %s"
				      (or cmdline "")
				      (org-babel-process-file-name in-file)
				      (org-babel-process-file-name out-file)))
		    (`mssql (format "sqlcmd %s -s \"\t\" %s -i %s -o %s"
				    (or cmdline "")
				    (org-babel-sql-dbstring-mssql
				     dbhost dbuser dbpassword database)
				    (org-babel-sql-convert-standard-filename
				     (org-babel-process-file-name in-file))
				    (org-babel-sql-convert-standard-filename
				     (org-babel-process-file-name out-file))))
                    (`mysql (format "mysql %s %s %s < %s > %s"
				    (org-babel-sql-dbstring-mysql
				     dbhost dbport dbuser dbpassword database)
				    (if colnames-p "" "-N")
				    (or cmdline "")
				    (org-babel-process-file-name in-file)
				    (org-babel-process-file-name out-file)))
		    (`postgresql (format
				  "%spsql --set=\"ON_ERROR_STOP=1\" %s -A -P \
footer=off -F \"\t\"  %s -f %s -o %s %s"
				  (if dbpassword
				      (format "PGPASSWORD=%s " dbpassword)
				    "")
				  (if colnames-p "" "-t")
				  (org-babel-sql-dbstring-postgresql
				   dbhost dbport dbuser database)
				  (org-babel-process-file-name in-file)
				  (org-babel-process-file-name out-file)
				  (or cmdline "")))
		    (`sqsh (format "sqsh %s %s -i %s -o %s -m csv"
				   (or cmdline "")
				   (org-babel-sql-dbstring-sqsh
				    dbhost dbuser dbpassword database)
				   (org-babel-sql-convert-standard-filename
				    (org-babel-process-file-name in-file))
				   (org-babel-sql-convert-standard-filename
				    (org-babel-process-file-name out-file))))
		    (`vertica (format "vsql %s -f %s -o %s %s"
				      (org-babel-sql-dbstring-vertica
				       dbhost dbport dbuser dbpassword database)
				      (org-babel-process-file-name in-file)
				      (org-babel-process-file-name out-file)
				      (or cmdline "")))
                    (`oracle (format
			      "sqlplus -s %s < %s > %s"
			      (org-babel-sql-dbstring-oracle
			       dbhost dbport dbuser dbpassword database)
			      (org-babel-process-file-name in-file)
			      (org-babel-process-file-name out-file)))
                    (_ (error "No support for the %s SQL engine" engine)))))
    (with-temp-file in-file
      (insert
       (pcase (intern engine)
	 (`dbi "/format partbox\n")
         (`oracle "SET PAGESIZE 50000
SET NEWPAGE 0
SET TAB OFF
SET SPACE 0
SET LINESIZE 9999
SET TRIMOUT ON TRIMSPOOL ON
SET ECHO OFF
SET FEEDBACK OFF
SET VERIFY OFF
SET HEADING ON
SET MARKUP HTML OFF SPOOL OFF
SET COLSEP '|'

")
	 ((or `mssql `sqsh) "SET NOCOUNT ON

")
	 (`vertica "\\a\n")
	 (_ ""))
       (org-babel-expand-body:sql body params)
       ;; "sqsh" requires "go" inserted at EOF.
       (if (string= engine "sqsh") "\ngo" "")))
    (org-babel-eval command "")
    (org-babel-result-cond result-params
      (with-temp-buffer
	(progn (insert-file-contents-literally out-file) (buffer-string)))
      (with-temp-buffer
	(cond
	  ((memq (intern engine) '(dbi mysql postgresql sqsh vertica))
	   ;; Add header row delimiter after column-names header in first line
	   (cond
	     (colnames-p
	      (with-temp-buffer
	        (insert-file-contents out-file)
	        (goto-char (point-min))
	        (forward-line 1)
	        (insert "\n")
                (org-tweaks--current-buffer-to-org-table)
	        (write-file out-file)))))
	  (t
	   ;; Need to figure out the delimiter for the header row
	   (with-temp-file out-file
	     (insert-file-contents out-file)
	     (goto-char (point-min))
	     (when (re-search-forward "^\\(-+\\)[^-]" nil t)
	       (setq header-delim (match-string-no-properties 1)))
	     (goto-char (point-max))
	     (forward-char -1)
	     (while (looking-at "\n")
	       (delete-char 1)
	       (goto-char (point-max))
	       (forward-char -1))
             (goto-char (point-min)))))

        (find-file-other-window out-file)
        out-file))))
