;;; repos.el --- Git repository dashboard (Haskell backend) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'ol)

(declare-function magit-status "magit-status")

;;; Backend

(defvar repos--backend-source-dir
  (expand-file-name "src/repos" user-emacs-directory)
  "Directory containing the repos Haskell source.")

(defvar repos--backend
  (expand-file-name "repos" repos--backend-source-dir)
  "Path to the repos Haskell binary.")

(defun repos--ensure-backend ()
  "Ensure the backend binary exists. Offer to build it if missing."
  (unless (file-executable-p repos--backend)
    (if (y-or-n-p "repos binary not found. Build it? ")
        (let ((default-directory repos--backend-source-dir))
          (message "Building repos...")
          (let ((exit-code (call-process "cabal" nil "*repos-build*" nil
                                         "install" "-O2"
                                         "--enable-executable-stripping"
                                         "--install-method=copy"
                                         (concat "--installdir=" repos--backend-source-dir)
                                         "--overwrite-policy=always")))
            (if (= exit-code 0)
                (message "repos built successfully")
              (pop-to-buffer "*repos-build*")
              (error "repos build failed (exit %d)" exit-code))))
      (user-error "repos is required"))))

(defun repos--call-sync (command &rest args)
  "Call the backend synchronously with COMMAND and ARGS. Return parsed JSON."
  (repos--ensure-backend)
  (with-temp-buffer
    (let ((exit-code (apply #'call-process repos--backend nil t nil command args)))
      (unless (= exit-code 0)
        (error "repos %s failed (exit %d): %s" command exit-code (buffer-string)))
      (goto-char (point-min))
      (json-read))))

(defun repos--call-async (command args callback)
  "Call the backend asynchronously. CALLBACK receives parsed JSON on completion."
  (repos--ensure-backend)
  (let ((buf (generate-new-buffer " *repos*")))
    (set-process-sentinel
     (apply #'start-process "repos" buf repos--backend command args)
     (lambda (process _event)
       (if (not (eq (process-exit-status process) 0))
           (progn
             (message "repos %s failed" command)
             (kill-buffer (process-buffer process)))
         (let ((json (with-current-buffer (process-buffer process)
                       (goto-char (point-min))
                       (condition-case nil (json-read) (error nil)))))
           (kill-buffer (process-buffer process))
           (when json (funcall callback json))))))))

;;; Buffer & Mode

(defvar repos-dashboard-buffer-name "*repos*"
  "Name of the repository dashboard buffer.")

(defvar repos-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'end-of-line)
    (define-key map "a" #'beginning-of-line)
    (define-key map "n" #'org-next-visible-heading)
    (define-key map "p" #'org-previous-visible-heading)
    (define-key map "f" #'repos-pull-repo)
    (define-key map "F" #'repos-pull-all)
    (define-key map "b" #'backward-char)
    (define-key map "g" #'repos-refresh)
    (define-key map "G" #'repos-refresh-all)
    (define-key map "+" #'repos-add-repo)
    (define-key map "/" #'repos-search)
    (define-key map "c" #'repos-clone-repo)
    (define-key map "C" #'repos-clone-all-missing)
    (define-key map (kbd "RET") #'repos-open-repo)
    (define-key map "^" #'repos-cycle-sort)
    (define-key map "~" #'repos-toggle-sort-direction)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `repos-dashboard-mode'.")

(define-derived-mode repos-dashboard-mode org-mode "Repos"
  "Read-only org-mode dashboard for monitoring git repositories."
  (setq-local org-todo-keyword-faces
              '(("CHECKING"   . (:foreground "yellow"))
                ("FETCHING"   . (:foreground "yellow"))
                ("UP_TO_DATE" . (:foreground "green"))
                ("BEHIND"     . (:foreground "#e67e22"))
                ("MODIFIED"   . (:foreground "#749AF7"))
                ("UNTRACKED"  . (:foreground "#565f89"))
                ("MISSING"    . (:foreground "#9b59b6"))
                ("ERROR"      . (:foreground "#c0392b"))))
  (setq buffer-read-only t))

;;; Repository Configuration

(defconst repos--header
  "#+TODO: CHECKING FETCHING | BEHIND MODIFIED MISSING ERROR UNTRACKED UP_TO_DATE"
  "Static header for the dashboard.")

(defvar repos-file
  (expand-file-name "repos" user-emacs-directory)
  "File where `repos-list' is persisted.")

(defvar repos-extra-files nil
  "List of additional files to load repos from.")

(defvar repos-list '(("~/.emacs.d"))
  "Alist of (PATH . REMOTE-URL) for git repositories to monitor.")

(defvar repos--extra-paths nil
  "Set of abbreviated paths loaded from extra files.")

;;; Backward compatibility
(defvaralias 'scratch-repos 'repos-list)
(defvaralias 'scratch-repos-extra-files 'repos-extra-files)

;;; Accessors

(defun repos--path (entry)
  "Return the local path from a repos ENTRY."
  (if (consp entry) (car entry) entry))

(defun repos--remote (entry)
  "Return the remote URL from a repos ENTRY, or nil."
  (when (consp entry) (cdr entry)))

(defun repos--abbrev (path)
  "Abbreviate PATH for display."
  (abbreviate-file-name (expand-file-name path)))

;;; Persistence

(defun repos--write-file (file repos)
  "Write REPOS alist to FILE."
  (with-temp-file file
    (insert ";; -*- lexical-binding: t; -*-\n")
    (insert ";; Monitored git repositories.\n\n")
    (pp `(setq repos-list ',repos) (current-buffer))))

(defun repos--save ()
  "Write repos to file, excluding extra repos."
  (repos--write-file repos-file
                     (seq-remove (lambda (e) (member (repos--abbrev (repos--path e))
                                                     repos--extra-paths))
                                 repos-list)))

(defun repos--read-from-file (file)
  "Read the repos alist from FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (repos)
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (when (and (listp form) (eq (car form) 'setq)
                           (memq (cadr form) '(repos-list scratch-repos)))
                  (setq repos (eval (caddr form) t)))))
          (end-of-file nil))
        repos))))

(defvar repos--legacy-file
  (expand-file-name "scratch-repos" user-emacs-directory))

(defun repos--load ()
  "Load repos from primary file and extra files."
  (let ((file (if (file-exists-p repos-file) repos-file repos--legacy-file)))
    (when (file-exists-p file)
      (setq repos-list (or (repos--read-from-file file) repos-list))))
  (setq repos--extra-paths nil)
  (dolist (file repos-extra-files)
    (let ((extra (repos--read-from-file file)))
      (dolist (e extra)
        (push (repos--abbrev (repos--path e)) repos--extra-paths))
      (setq repos-list (append repos-list extra)))))

;;; Status Tracking

(defvar repos--statuses (make-hash-table :test 'equal)
  "Hash table mapping repo path to its status alist (parsed JSON).")

(defvar repos--pending 0
  "Number of repos still being checked.")

(defun repos--status-get (path key)
  "Get KEY from the status of PATH."
  (cdr (assq key (gethash path repos--statuses))))

;;; Sorting

(defvar repos--sort-methods '(status name path))
(defvar repos--current-sort 'status)
(defvar repos--sort-ascending t)

(defconst repos--todo-order
  '("CHECKING" "FETCHING" "BEHIND" "MODIFIED" "MISSING" "ERROR" "UP_TO_DATE" "UNTRACKED"))

(defun repos--todo-kw (path)
  "Return the TODO keyword for PATH."
  (let* ((status (gethash path repos--statuses))
         (state (cdr (assq 'state status)))
         (behind (or (cdr (assq 'behind status)) 0))
         (mod (or (cdr (assq 'modified status)) 0))
         (untracked (or (cdr (assq 'untracked status)) 0)))
    (cond
     ((equal state "missing") "MISSING")
     ((or (null state) (equal state "checking")) "CHECKING")
     ((equal state "fetching") "FETCHING")
     ((equal state "error") "ERROR")
     ((> behind 0) "BEHIND")
     ((> mod 0) "MODIFIED")
     ((> untracked 0) "UNTRACKED")
     (t "UP_TO_DATE"))))

(defun repos--status-priority (path)
  (or (cl-position (repos--todo-kw path) repos--todo-order :test #'equal)
      (length repos--todo-order)))

(defun repos--sorted-entries ()
  "Return repos sorted by current method."
  (let* ((entries (copy-sequence repos-list))
         (sorted
          (pcase repos--current-sort
            ('name (sort entries (lambda (a b)
                                   (string< (file-name-nondirectory (directory-file-name (repos--path a)))
                                            (file-name-nondirectory (directory-file-name (repos--path b)))))))
            ('path (sort entries (lambda (a b) (string< (repos--path a) (repos--path b)))))
            ('status (sort entries (lambda (a b)
                                     (< (repos--status-priority (repos--abbrev (repos--path a)))
                                        (repos--status-priority (repos--abbrev (repos--path b)))))))
            (_ entries))))
    (if repos--sort-ascending sorted (nreverse sorted))))

(defun repos--sort-label ()
  (concat (propertize (symbol-name repos--current-sort) 'face '(:foreground "#749AF7"))
          " "
          (propertize (if repos--sort-ascending "asc" "desc") 'face '(:foreground "#9ece6a"))))

;;;###autoload
(defun repos-cycle-sort ()
  "Cycle sort methods."
  (interactive)
  (let* ((idx (cl-position repos--current-sort repos--sort-methods))
         (next (nth (mod (1+ (or idx 0)) (length repos--sort-methods)) repos--sort-methods)))
    (if (eq next repos--current-sort)
        (setq repos--sort-ascending (not repos--sort-ascending))
      (setq repos--current-sort next repos--sort-ascending t))
    (repos--render)
    (message "Sort: %s" (repos--sort-label))))

;;;###autoload
(defun repos-toggle-sort-direction ()
  "Toggle sort direction."
  (interactive)
  (setq repos--sort-ascending (not repos--sort-ascending))
  (repos--render)
  (message "Sort: %s" (repos--sort-label)))

;;; Rendering

(defun repos--format-entry (path)
  "Return org text for repo at PATH from its stored status."
  (let* ((status (gethash path repos--statuses))
         (state (or (cdr (assq 'state status)) "checking"))
         (behind (or (cdr (assq 'behind status)) 0))
         (local-desc (cdr (assq 'local status)))
         (err (cdr (assq 'error status)))
         (kw (repos--todo-kw path)))
    (concat
     (format "** %s %s\n" kw path)
     (cond
      ((equal state "missing")
       "   - Directory not found. Press =c= to clone, =C= to clone all.\n")
      ((equal state "ready")
       (concat
        (when (> behind 0)
          (format "   - %d commit%s behind upstream\n" behind (if (> behind 1) "s" "")))
        (when local-desc
          (format "   - Uncommitted: %s\n" local-desc))))
      ((equal state "error")
       (format "   %s\n" (or err "unknown")))))))

(defun repos--render ()
  "Full re-render of the dashboard."
  (let ((buf (get-buffer repos-dashboard-buffer-name)))
    (when buf
      (with-current-buffer buf
        (unless (derived-mode-p 'repos-dashboard-mode)
          (repos-dashboard-mode))
        (let ((inhibit-read-only t)
              (pt (point)))
          (erase-buffer)
          (insert repos--header)
          (insert (format "\n\n* Repository Status [/]  (sort: %s)\n\n" (repos--sort-label)))
          (dolist (entry (repos--sorted-entries))
            (insert (repos--format-entry (repos--abbrev (repos--path entry)))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+" nil t)
            (org-ctrl-c-ctrl-c))
          (goto-char (min pt (point-max))))))))

(defun repos--update-in-place (path)
  "Replace the heading for PATH in the dashboard."
  (let ((buf (get-buffer repos-dashboard-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (new-text (repos--format-entry path)))
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward (format "^\\*\\* [A-Z_]+ %s$" (regexp-quote path)) nil t)
                (let ((start (line-beginning-position))
                      (end (or (save-excursion
                                 (when (re-search-forward "^\\*\\* " nil t)
                                   (line-beginning-position)))
                               (point-max))))
                  (delete-region start end)
                  (goto-char start)
                  (insert new-text))
              (goto-char (point-max))
              (insert new-text))
            (goto-char (point-min))
            (when (re-search-forward "^\\* Repository Status" nil t)
              (org-update-statistics-cookies nil))))))))

(defun repos--update (path)
  "Update PATH. Re-renders fully when last pending repo resolves."
  (repos--update-in-place path)
  (let ((kw (repos--todo-kw path)))
    (when (and (> repos--pending 0)
               (not (member kw '("CHECKING" "FETCHING"))))
      (setq repos--pending (1- repos--pending))
      (when (= repos--pending 0)
        (repos--render)))))

;;; Async Git Operations (via Haskell backend)

(defun repos--entry (repo)
  "Find the repos entry for REPO."
  (seq-find (lambda (e) (equal (repos--abbrev (repos--path e)) (repos--abbrev repo)))
            repos-list))

(defun repos--fetch (repo)
  "Async fetch + status for REPO via the Haskell backend."
  (let ((path (repos--abbrev repo)))
    (puthash path '((state . "fetching")) repos--statuses)
    (repos--update path)
    (repos--call-async
     "status" (list (expand-file-name path))
     (lambda (json)
       (puthash path json repos--statuses)
       (repos--update path)))))

(defun repos--fetch-quick (repo)
  "Async status (no fetch) for REPO via the Haskell backend."
  (let ((path (repos--abbrev repo)))
    (puthash path '((state . "checking")) repos--statuses)
    (repos--update path)
    (repos--call-async
     "status-quick" (list (expand-file-name path))
     (lambda (json)
       (puthash path json repos--statuses)
       (repos--update path)))))

(defun repos--pull (repo)
  "Pull REPO by fetching with the backend (which does git fetch)."
  (repos--fetch repo))

(defun repos--clone-async (path remote target)
  "Clone REMOTE into TARGET for repo at PATH."
  (puthash path '((state . "checking")) repos--statuses)
  (repos--update path)
  (repos--call-async
   "clone" (list remote target)
   (lambda (json)
     (puthash path json repos--statuses)
     (repos--update path))))

;;; Helpers

(defun repos--heading-path ()
  "Return the repo path from the current org headline."
  (when-let ((heading (org-get-heading t t t t)))
    (when (string-match "\\` *\\(.*?\\)\\(?: on \\|$\\)" heading)
      (string-trim (match-string 1 heading)))))

(defun repos--path-at-point ()
  (save-excursion
    (when (and (derived-mode-p 'org-mode)
               (ignore-errors (org-back-to-heading t) t))
      (repos--heading-path))))

(defun repos--choose-file ()
  (if (null repos-extra-files)
      repos-file
    (let* ((all (cons repos-file repos-extra-files))
           (choices (mapcar #'abbreviate-file-name all)))
      (expand-file-name (completing-read "Save to repos file: " choices nil t)))))

(defun repos--append-to-file (file new-entries)
  (let ((merged (append (repos--read-from-file file) new-entries)))
    (repos--write-file file merged)
    (unless (equal file repos-file)
      (dolist (e new-entries)
        (push (repos--abbrev (repos--path e)) repos--extra-paths)))))

;;; Interactive Commands

;;;###autoload
(defun repos-search ()
  "Jump to a repository headline via completing-read."
  (interactive)
  (let* ((paths (mapcar (lambda (e) (repos--abbrev (repos--path e))) repos-list))
         (choice (completing-read "Repo: " paths nil t)))
    (goto-char (point-min))
    (unless (re-search-forward
             (format "^\\*\\* [A-Z_]+ %s$" (regexp-quote choice)) nil t)
      (message "Not found: %s" choice))
    (beginning-of-line)))

;;;###autoload
(defun repos-open-repo ()
  "Open magit-status for the repository at point."
  (interactive)
  (let ((path (repos--heading-path)))
    (unless path (user-error "Not on a repo headline"))
    (let ((default-directory (file-name-as-directory (expand-file-name path))))
      (magit-status))))

;;;###autoload
(defun repos-add-repo (dir)
  "Add DIR to monitored repositories."
  (interactive "DDirectory: ")
  (let* ((found (repos--call-sync "discover" (expand-file-name dir)))
         (added (cl-remove nil
                           (mapcar (lambda (d)
                                     (let ((path (repos--abbrev d)))
                                       (unless (seq-find (lambda (e) (equal (repos--abbrev (repos--path e)) path))
                                                         repos-list)
                                         (let ((remote (repos--call-sync "remote" (expand-file-name d))))
                                           (setq repos-list
                                                 (append repos-list
                                                         (list (cons path (if (and remote (not (equal remote :json-false))) remote nil)))))
                                           (repos--fetch path)
                                           path))))
                                   (append found nil))))) ;; coerce vector to list
    (unless found (user-error "No git repositories found in %s" (repos--abbrev dir)))
    (when added
      (let* ((target (repos--choose-file))
             (new-entries (mapcar (lambda (p) (assoc p repos-list)) added)))
        (if (equal target repos-file)
            (repos--save)
          (repos--append-to-file target new-entries)))
      (message "Added %d repo%s" (length added) (if (= 1 (length added)) "" "s")))))

;;;###autoload
(defun repos-add-current-repo ()
  "Add the current buffer's git repository to monitored repos.
Prompts for which file to save to when `repos-extra-files' is set."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   (user-error "Not inside a git repository")))
         (path (repos--abbrev root)))
    (when (seq-find (lambda (e) (equal (repos--abbrev (repos--path e)) path))
                    repos-list)
      (user-error "Already monitored: %s" path))
    (let* ((remote (repos--call-sync "remote" (expand-file-name root)))
           (entry (cons path (and remote (not (equal remote :json-false)) remote)))
           (target (repos--choose-file)))
      (setq repos-list (append repos-list (list entry)))
      (if (equal target repos-file)
          (repos--save)
        (repos--append-to-file target (list entry)))
      (repos--fetch path)
      (message "Added %s" path))))

;;;###autoload
(defun repos-clone-repo ()
  "Clone the repository at point."
  (interactive)
  (let* ((path (repos--heading-path))
         (_ (unless path (user-error "Not on a repo headline")))
         (status (gethash path repos--statuses))
         (_ (unless (equal (cdr (assq 'state status)) "missing")
              (user-error "Repository is not missing")))
         (entry (repos--entry path))
         (remote (repos--remote entry))
         (_ (unless remote (user-error "No remote URL for %s" path)))
         (target (expand-file-name path)))
    (when (y-or-n-p (format "Clone %s to %s? " remote target))
      (repos--clone-async path remote target))))

;;;###autoload
(defun repos-clone-all-missing ()
  "Clone all missing repositories."
  (interactive)
  (let ((missing (cl-loop for entry in repos-list
                           for path = (repos--abbrev (repos--path entry))
                           for status = (gethash path repos--statuses)
                           for remote = (repos--remote entry)
                           when (and (equal (cdr (assq 'state status)) "missing") remote)
                           collect (list path remote (expand-file-name path)))))
    (if (null missing)
        (message "No missing repositories")
      (when (y-or-n-p (format "Clone %d missing repo%s? " (length missing) (if (= 1 (length missing)) "" "s")))
        (dolist (m missing)
          (repos--clone-async (nth 0 m) (nth 1 m) (nth 2 m)))))))

;;;###autoload
(defun repos-refresh ()
  "Refresh repo at point, or all repos."
  (interactive)
  (let ((path (repos--path-at-point)))
    (if path
        (progn
          (setq repos--pending (1+ repos--pending))
          (repos--fetch path))
      (setq repos--pending (length repos-list))
      (dolist (entry repos-list)
        (repos--fetch (repos--path entry))))))

;;;###autoload
(defun repos-refresh-all ()
  "Refresh all repos."
  (interactive)
  (setq repos--pending (length repos-list))
  (dolist (entry repos-list)
    (repos--fetch (repos--path entry))))

;;;###autoload
(defun repos-pull-repo ()
  "Pull the repository at point."
  (interactive)
  (let ((path (repos--path-at-point)))
    (unless path (user-error "Not on a repo headline"))
    (repos--pull path)))

;;;###autoload
(defun repos-pull-all ()
  "Pull all repositories."
  (interactive)
  (dolist (entry repos-list)
    (repos--pull (repos--path entry))))

;;;###autoload
(defun repos-dashboard ()
  "Open the repository dashboard."
  (interactive)
  (let ((buf (get-buffer-create repos-dashboard-buffer-name)))
    (pop-to-buffer buf)
    (unless (derived-mode-p 'repos-dashboard-mode)
      (repos-dashboard-mode))
    ;; Seed all repos as CHECKING so the initial render has headings
    (dolist (entry repos-list)
      (let ((path (repos--abbrev (repos--path entry))))
        (unless (gethash path repos--statuses)
          (puthash path (list :state 'checking) repos--statuses))))
    (repos--render)
    (repos-refresh)))

(global-set-key (kbd "C-x y p") #'repos-dashboard)

(provide 'repos)
;;; repos.el ends here
