;;; mijn-package-usage.el --- Local package usage evidence -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)
(require 'json)

(defvar mijn-required-packages)
(declare-function table-view-display "table-view")
(defgroup package-usage nil "Local package usage evidence." :group 'package)

(defvar package-usage-file (expand-file-name "var/package-usage.eld" user-emacs-directory))
(defvar package-usage--session (format "%s-%s-%s" (system-name) (emacs-pid) (float-time)))
(defvar package-usage--events nil)
(defvar package-usage--timer nil)
(defvar package-usage--autoload nil)
(defvar package-usage-mode nil)

(defun package-usage--owner (file)
  "Return the installed package owning FILE."
  (when (and (stringp file) (file-name-absolute-p file))
    (cl-loop for (name . descriptors) in package-alist
             thereis (and (cl-some
                           (lambda (desc)
                             (let ((dir (package-desc-dir desc)))
                               (and dir (file-in-directory-p file dir))))
                           descriptors)
                          name))))

(defun package-usage--record (name &optional command kind)
  "Record NAME, optional COMMAND and load KIND in this session."
  (when name
    (let* ((now (float-time))
           (key (cons name package-usage--session))
           (entry (assoc key package-usage--events)))
      (unless entry
        (setq entry (list key :first now :last now :commands nil :loads nil))
        (push entry package-usage--events))
      (setcdr entry (plist-put (cdr entry) :last now))
      (when command
        (setcdr entry (plist-put (cdr entry) :commands
                                (cl-adjoin command (plist-get (cdr entry) :commands)))))
      (when kind
        (setcdr entry (plist-put (cdr entry) :loads
                                (cl-adjoin kind (plist-get (cdr entry) :loads))))))))

(defun package-usage--loaded (file)
  (package-usage--record (package-usage--owner file) nil
                         (if package-usage--autoload 'autoload
                           (if after-init-time 'explicit 'eager))))

(defun package-usage--autoloading (original &rest args)
  (let ((package-usage--autoload t)) (apply original args)))

(defun package-usage--command (original command &rest args)
  (let ((result (apply original command args)))
    (when (symbolp command)
      (package-usage--record (package-usage--owner (symbol-file command 'defun)) command))
    result))

(defun package-usage--read-file (file)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (read (current-buffer)))))

(defun package-usage--read ()
  (let ((data (package-usage--read-file package-usage-file))
        (journals (concat package-usage-file ".sessions")))
    (when (file-directory-p journals)
      (dolist (file (directory-files journals t "\\.eld\\'"))
        (setq data (package-usage--merge data (package-usage--read-file file)))))
    data))

(defun package-usage--write (file data)
  (let ((temp (make-temp-file (concat file "."))))
    (unwind-protect
        (progn
          (with-temp-file temp (prin1 data (current-buffer)))
          (rename-file temp file t))
      (when (file-exists-p temp) (delete-file temp)))))

(defun package-usage--merge (disk events)
  "Merge per-session EVENTS into DISK without counting a session twice."
  (dolist (entry events disk)
    (let ((old (assoc (car entry) disk)))
      (if old (setcdr old (copy-tree (cdr entry)))
        (push (copy-tree entry) disk)))))

(defun package-usage-save ()
  "Persist this session, then merge evidence under an exclusive lock.
Retained per-session journals preserve evidence if another writer owns the
merge lock at shutdown.  Reports read both the merged state and journals."
  (interactive)
  (let ((lock (concat package-usage-file ".lock"))
        (journals (concat package-usage-file ".sessions/"))
        acquired)
    (unwind-protect
        (condition-case err
            (progn
              (make-directory journals t)
              (package-usage--write
               (concat journals (secure-hash 'sha256 package-usage--session) ".eld")
               package-usage--events)
              (make-directory lock)
              (setq acquired t)
              (package-usage--write package-usage-file (package-usage--read)))
          (file-already-exists nil)
          (error (message "Package usage persistence failed: %s" (error-message-string err))))
      (when acquired (delete-directory lock)))))

;;;###autoload
(defun package-usage-report ()
  "Display local package usage evidence with table-view."
  (interactive)
  (require 'table-view)
  (let* ((data (package-usage--merge (package-usage--read) package-usage--events))
         (origins-file (expand-file-name "origins.json" package-user-dir))
         (origins (when (file-exists-p origins-file)
                    (let ((json-object-type 'alist) (json-key-type 'string))
                      (json-read-file origins-file))))
         rows)
    (dolist (package package-alist)
      (let* ((name (car package))
             (events (cl-remove-if-not (lambda (e) (eq (caar e) name)) data))
             (commands (delete-dups (apply #'append (mapcar (lambda (e) (copy-sequence (plist-get (cdr e) :commands))) events))))
             (loads (delete-dups (apply #'append (mapcar (lambda (e) (copy-sequence (plist-get (cdr e) :loads))) events))))
             (last (apply #'max 0 (mapcar (lambda (e) (plist-get (cdr e) :last)) events))))
        (push `((id . ,(symbol-name name)) (cells . ((package . ,(symbol-name name))
                (archive . ,(or (cdr (assoc (package-desc-full-name (cadr package)) origins))
                                (package-desc-archive (cadr package)) "unknown"))
                (sessions . ,(cl-count-if (lambda (e) (plist-get (cdr e) :loads)) events))
                (commands . ,(length commands))
                (load . ,(mapconcat #'symbol-name loads ", "))
                (last . ,(if (> last 0) (format-time-string "%F" (seconds-to-time last)) "never"))
                (declared . ,(if (and (boundp 'mijn-required-packages) (memq name mijn-required-packages)) "yes" "no"))
                (candidate . ,(if commands "" "review passive role"))))) rows)))
    (let ((buffer
           (table-view-display
            "*Package usage*"
            `((columns . ,(mapcar (lambda (key) `((key . ,(symbol-name key)) (header . ,(symbol-name key))))
                                   '(package archive sessions commands load last declared candidate)))
              (rows . ,(nreverse rows))) nil)))
      (with-current-buffer buffer
        (setq-local header-line-format
                    "Local evidence: eager requires count as loaded; themes/fonts/UI may be used without commands. Candidates need review."))
      (pop-to-buffer buffer))))

;;;###autoload
(define-minor-mode package-usage-mode
  "Collect package usage locally, without network access."
  :global t
  :group 'package-usage
  (if package-usage-mode
      (progn
        (dolist (entry load-history)
          (package-usage--record (package-usage--owner (car entry)) nil 'eager))
        (add-hook 'after-load-functions #'package-usage--loaded)
        (advice-add 'autoload-do-load :around #'package-usage--autoloading)
        (advice-add 'call-interactively :around #'package-usage--command)
        (add-hook 'kill-emacs-hook #'package-usage-save)
        (unless package-usage--timer
          (setq package-usage--timer (run-with-idle-timer 300 t #'package-usage-save))))
    (remove-hook 'after-load-functions #'package-usage--loaded)
    (advice-remove 'autoload-do-load #'package-usage--autoloading)
    (advice-remove 'call-interactively #'package-usage--command)
    (remove-hook 'kill-emacs-hook #'package-usage-save)
    (when package-usage--timer (cancel-timer package-usage--timer))
    (setq package-usage--timer nil)
    (package-usage-save)))

(provide 'mijn-package-usage)
;;; mijn-package-usage.el ends here
