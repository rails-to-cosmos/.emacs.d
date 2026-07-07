;;; repos.el --- Git repository dashboard (Haskell backend) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'magit)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "~/sync/stuff/table-view")))
(require 'table-view)

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
                                         "--enable-split-sections"
                                         "--install-method=copy"
                                         (concat "--installdir=" repos--backend-source-dir)
                                         "--overwrite-policy=always")))
            (if (= exit-code 0)
                (message "repos built successfully")
              (switch-to-buffer "*repos-build*")
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

(defun repos--call-batch (command paths per-result-callback &optional done-callback)
  "Run the backend `batch' subcommand in a single process.
COMMAND is \"status\" or \"status-quick\". PATHS is a list of
absolute paths. PER-RESULT-CALLBACK is invoked for each repo
as (PATH STATUS) as results stream in. DONE-CALLBACK, when non-nil,
runs after the process exits."
  (repos--ensure-backend)
  (let* ((process-connection-type nil) ;; pipe, not pty — so `process-send-eof' actually closes stdin
         (buf  (generate-new-buffer " *repos-batch*"))
         (proc (start-process "repos-batch" buf repos--backend "batch"))
         (pending ""))
    (set-process-filter
     proc
     (lambda (_process output)
       (let* ((combined (concat pending output))
              (lines    (split-string combined "\n"))
              (tail     (car (last lines)))
              (complete (butlast lines)))
         (setq pending tail)
         (dolist (line complete)
           (unless (string-empty-p line)
             (condition-case _err
                 (let* ((parsed (json-read-from-string line))
                        (path   (cdr (assq 'path parsed)))
                        (status (cdr (assq 'status parsed))))
                   (when path
                     (funcall per-result-callback path status)))
               (error nil)))))))
    (set-process-sentinel
     proc
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (kill-buffer (process-buffer process))
         (when done-callback (funcall done-callback)))))
    (process-send-string
     proc (json-encode `((command . ,command) (repos . ,(vconcat paths)))))
    (process-send-eof proc)))

;;; Repository Configuration

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

(defvar repos--status-change-functions nil
  "Abnormal hook run with one argument PATH whenever a repo's status changes.
Views (the table view) add observers here so that every state transition
-- pull, fetch, refresh, clone -- reflects live.  `repos--update' is the
sole place this fires.")

(defun repos--status-get (path key)
  "Get KEY from the status of PATH."
  (cdr (assq key (gethash path repos--statuses))))

(defun repos--todo-kw (path)
  "Return the status keyword for PATH."
  (let* ((status (gethash path repos--statuses))
         (state (cdr (assq 'state status)))
         (behind (or (cdr (assq 'behind status)) 0))
         (staged (or (cdr (assq 'staged status)) 0))
         (mod (or (cdr (assq 'modified status)) 0))
         (untracked (or (cdr (assq 'untracked status)) 0))
         (conflicts (or (cdr (assq 'conflicts status)) 0)))
    (cond
     ((equal state "missing") "MISSING")
     ((or (null state) (equal state "checking")) "CHECKING")
     ((equal state "fetching") "FETCHING")
     ((equal state "error") "ERROR")
     ((> behind 0) "BEHIND")
     ((or (> mod 0) (> staged 0) (> conflicts 0)) "MODIFIED")
     ((> untracked 0) "UNTRACKED")
     (t "UP_TO_DATE"))))

(defun repos--update (path)
  "Notify observers that PATH's status changed.
The sole status-change choke point; views observe via
`repos--status-change-functions'."
  (run-hook-with-args 'repos--status-change-functions path))

;;; Async Git Operations (via Haskell backend)

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

(defun repos--fetch-many (repos)
  "Async fetch + status for REPOS in a single backend process."
  (when repos
    (let ((paths (mapcar #'repos--abbrev repos)))
      (dolist (path paths)
        (puthash path '((state . "fetching")) repos--statuses)
        (repos--update path))
      (repos--call-batch
       "status" (mapcar #'expand-file-name paths)
       (lambda (path-abs status)
         (let ((path (repos--abbrev path-abs)))
           (puthash path status repos--statuses)
           (repos--update path)))))))

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

(defun repos--pull (repo &optional on-exit)
  "Pull REPO: run git pull --ff-only, then refresh status.
ON-EXIT, when non-nil, is called with no arguments once the git
process exits (success or failure), so callers can manage concurrency.
An absent directory is left in the MISSING state rather than pulled."
  (let* ((path (repos--abbrev repo))
         (dir (expand-file-name path)))
    (if (not (file-directory-p dir))
        ;; Preserve MISSING (and its clone affordances) instead of running
        ;; git in a directory that isn't there.
        (progn
          (puthash path '((state . "missing")) repos--statuses)
          (repos--update path)
          (when on-exit (funcall on-exit)))
      (puthash path '((state . "fetching")) repos--statuses)
      (repos--update path)
      (let ((buf (generate-new-buffer " *repos-pull*")))
        (set-process-sentinel
         (start-process "repos-pull" buf "git" "-C" dir "pull" "--ff-only")
         (lambda (process _event)
           (let ((exit-code (process-exit-status process)))
             (kill-buffer (process-buffer process))
             (if (= exit-code 0)
                 (progn
                   (message "repos: pulled %s" path)
                   (repos--fetch-quick repo))
               (puthash path `((state . "error")
                               (error . ,(format "git pull failed (exit %d)" exit-code)))
                        repos--statuses)
               (repos--update path))
             (when on-exit (funcall on-exit)))))))))

(defvar repos-pull-max-concurrency 4
  "Maximum number of concurrent `git pull' processes `repos--pull-many' runs.
Unlike status checks (a single batched backend process), each pull is a
real network fetch + merge, so a large `repos-list' is drained through a
bounded pool rather than launched all at once.")

(defun repos--pull-many (repos)
  "Pull each repo in REPOS via git pull --ff-only, then refresh status.
Drains REPOS through a pool of at most `repos-pull-max-concurrency'
concurrent git processes.  Each repo resolves independently and updates
its row live via `repos--update'."
  (let ((queue nil)
        (active 0))
    ;; Partition: absent dirs resolve to MISSING right away; only existing
    ;; dirs enter the pool, so every pooled `repos--pull' completes
    ;; asynchronously and `launch' is never re-entered from within its loop.
    (dolist (repo repos)
      (let ((dir (expand-file-name (repos--abbrev repo))))
        (if (file-directory-p dir)
            (push repo queue)
          (puthash (repos--abbrev repo) '((state . "missing")) repos--statuses)
          (repos--update (repos--abbrev repo)))))
    (setq queue (nreverse queue))
    (cl-labels ((launch ()
                  (while (and queue (< active repos-pull-max-concurrency))
                    (let ((repo (pop queue)))
                      (setq active (1+ active))
                      (repos--pull repo
                                   (lambda ()
                                     (setq active (1- active))
                                     (launch)))))))
      (launch))))

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

;;; Startup

(defun repos--migrate ()
  (when (and repos-list (stringp (car repos-list)))
    (setq repos-list (mapcar (lambda (path) (cons path nil)) repos-list))
    (repos--save)))

(defvar repos--loaded nil
  "Non-nil once `repos-list' has been populated from disk.")

(defun repos--ensure-loaded ()
  "Load repos and run the one-time migration on first call.
Idempotent — safe to call from every public entry point so the heavy
work happens lazily, on first user interaction, instead of at startup."
  (unless repos--loaded
    (repos--load)
    (repos--migrate)
    (setq repos--loaded t)))

;;; Interactive Commands

;;;###autoload
(defun repos-add-repo (dir)
  "Add DIR to monitored repositories."
  (interactive "DDirectory: ")
  (repos--ensure-loaded)
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
                                           path))))
                                   (append found nil))))) ;; coerce vector to list
    (unless found (user-error "No git repositories found in %s" (repos--abbrev dir)))
    (when added
      (let* ((target (repos--choose-file))
             (new-entries (mapcar (lambda (p) (assoc p repos-list)) added)))
        (if (equal target repos-file)
            (repos--save)
          (repos--append-to-file target new-entries)))
      (repos--fetch-many added)
      (message "Added %d repo%s" (length added) (if (= 1 (length added)) "" "s")))))

;;;###autoload
(defun repos-add-current-repo ()
  "Add the current buffer's git repository to monitored repos.
Prompts for which file to save to when `repos-extra-files' is set."
  (interactive)
  (repos--ensure-loaded)
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
(defun repos-clone-all-missing ()
  "Clone all missing repositories."
  (interactive)
  (repos--ensure-loaded)
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
(defun repos-refresh-all ()
  "Fetch every registered repo and update the table view."
  (interactive)
  (repos--ensure-loaded)
  (repos--fetch-many (mapcar #'repos--path repos-list)))

;;;###autoload
(defun repos-pull-all ()
  "Pull all repositories (git pull --ff-only each), then refresh status."
  (interactive)
  (repos--ensure-loaded)
  (repos--pull-many (mapcar #'repos--path repos-list)))

;;; Table-view dashboard (generic table-view.el core)
;;
;; The Haskell `table' command supplies the SCHEMA (columns/actions/sort);
;; the rows are filled here from the registered repos, reusing the streaming
;; `repos--call-batch' and `repos--statuses'.  This is the repository
;; dashboard (C-x y p); status transitions reflect live via
;; `repos--status-change-functions'.

(defun repos-table--call-table (paths callback)
  "Call the backend `table' command with PATHS; CALLBACK gets the JSON string.
With PATHS nil this returns just the schema (no rows)."
  (repos--ensure-backend)
  (let* ((process-connection-type nil) ;; pipe, so `process-send-eof' closes stdin
         (buf (generate-new-buffer " *repos-table-proc*"))
         (proc (start-process "repos-table" buf repos--backend "table")))
    (set-process-sentinel
     proc
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((out (with-current-buffer (process-buffer process) (buffer-string)))
               (ok (= (process-exit-status process) 0)))
           (kill-buffer (process-buffer process))
           (if ok
               (funcall callback out)
             (message "repos-table: backend `table' failed (no view opened): %s"
                      (string-trim out)))))))
    (process-send-string proc (json-encode `((repos . ,(vconcat paths)))))
    (process-send-eof proc)))

(defun repos-table--row (path)
  "Build a `table-view' row alist for the repo at PATH (abbreviated)."
  (let* ((state     (repos--status-get path 'state))
         (kw        (repos--todo-kw path))
         (branch    (or (repos--status-get path 'branch) ""))
         (behind    (or (repos--status-get path 'behind) 0))
         (staged    (or (repos--status-get path 'staged) 0))
         (modified  (or (repos--status-get path 'modified) 0))
         (untracked (or (repos--status-get path 'untracked) 0))
         (conflicts (or (repos--status-get path 'conflicts) 0))
         ;; Note carries only what the numbers cannot: error text or the
         ;; clone hint for a missing repo.  Empty for a normal row.
         (note   (cond ((equal state "error")   (or (repos--status-get path 'error) ""))
                       ((equal state "missing") "clone with c")
                       (t ""))))
    `((id . ,path)
      (cells . ((state . ,kw)
                (name . ,path)
                (branch . ,branch)
                (behind . ,behind)
                (staged . ,staged)
                (modified . ,modified)
                (untracked . ,untracked)
                (conflicts . ,conflicts)
                (note . ,note))))))

(defun repos-table--fill (buffer)
  "Populate table-view BUFFER from the registered repos, streaming results."
  (let ((paths (mapcar (lambda (e) (repos--abbrev (repos--path e))) repos-list)))
    ;; Seed the full row set in one shot: every registered repo appears
    ;; immediately, and rows for repos no longer registered are dropped.
    (dolist (path paths)
      (unless (gethash path repos--statuses)
        (puthash path '((state . "checking")) repos--statuses)))
    (table-view-set-rows buffer (mapcar #'repos-table--row paths))
    ;; Refine each row in place as its real status streams back.
    (repos--call-batch
     "status" (mapcar #'expand-file-name paths)
     (lambda (path-abs status)
       (let ((path (repos--abbrev path-abs)))
         (puthash path status repos--statuses)
         (table-view-upsert-row buffer (repos-table--row path)))))))

(defun repos-table--open (id _row)
  "Open magit-status for the repo with ID (a path)."
  (when id
    (let ((default-directory (file-name-as-directory (expand-file-name id))))
      (magit-status))))

(defun repos-table--pull (id _row)
  "Pull the repo with ID; its row reflects each transition as it resolves."
  (when id
    (message "repos-table: pulling %s..." id)
    (repos--pull id)))

(defun repos-table--refresh (_id _row)
  "Re-fill the current table view from the registered repos."
  (table-view-refresh (current-buffer)))

(defun repos-table--on-status-change (path)
  "Reflect PATH's new status in the table view, when it is live.
Registered on `repos--status-change-functions' so transitions triggered
outside the table's own fill (pulls, clones, `repos-refresh-all') update
the row immediately."
  (let ((buf (get-buffer "*repos-table*")))
    (when buf
      (table-view-upsert-row buf (repos-table--row path)))))

;;;###autoload
(defun repos-table ()
  "Show the repository dashboard as a generic declarative table."
  (interactive)
  (repos--ensure-loaded)
  (repos-table--call-table
   nil ;; schema only; rows are filled by `repos-table--fill'
   (lambda (json)
     (let ((buf (table-view-display
                 "*repos-table*"
                 (table-view-parse json)
                 (list (cons "open"    #'repos-table--open)
                       (cons "pull"    #'repos-table--pull)
                       (cons "refresh" #'repos-table--refresh))
                 #'repos-table--fill)))
       ;; Observe status changes so pulls/clones reflect live; drop the
       ;; observer when the view is closed.
       (add-hook 'repos--status-change-functions #'repos-table--on-status-change)
       (with-current-buffer buf
         (add-hook 'kill-buffer-hook
                   (lambda ()
                     (remove-hook 'repos--status-change-functions
                                  #'repos-table--on-status-change))
                   nil t))))))

(global-set-key (kbd "C-x y p") #'repos-table)

(provide 'repos)
;;; repos.el ends here
