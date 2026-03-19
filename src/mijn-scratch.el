;;; my-scratch.el --- Scratch buffer setup and git dashboard -*- lexical-binding: t; -*-

(require 'ol)
(require 'magit)

(declare-function magit-status "magit-status")

(defvar scratch-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'end-of-line)
    (define-key map "a" #'beginning-of-line)
    (define-key map "n" #'org-next-visible-heading)
    (define-key map "p" #'org-previous-visible-heading)
    (define-key map "f" #'scratch-pull-repo)
    (define-key map "F" #'scratch-pull-all)
    (define-key map "b" #'backward-char)
    (define-key map "g" #'scratch-refresh)
    (define-key map "G" #'scratch-refresh-all)
    (define-key map "+" #'scratch-add-repo)
    (define-key map "/" #'scratch-search)
    (define-key map "c" #'scratch-clone-repo)
    (define-key map (kbd "RET") #'scratch-open-repo)
    map)
  "Keymap for `scratch-dashboard-mode'.")

(define-derived-mode scratch-dashboard-mode org-mode "Scratch-Dashboard"
  "Read-only org-mode dashboard for the *scratch* buffer."
  (setq-local org-todo-keyword-faces
              '(("CHECKING"   . (:foreground "yellow"))
                ("FETCHING"   . (:foreground "yellow"))
                ("UP_TO_DATE" . (:foreground "green"))
                ("BEHIND"     . (:foreground "#e67e22"))
                ("MODIFIED"   . (:foreground "#749AF7"))
                ("MISSING"    . (:foreground "#9b59b6"))
                ("ERROR"      . (:foreground "#c0392b"))))
  (setq buffer-read-only t))

(setq-default initial-major-mode 'fundamental-mode)

(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer) nil) t))

(add-hook 'kill-buffer-query-functions 'immortal-scratch)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; --- Git Repository Status Dashboard ---

(defconst scratch--header
  "\
# I've always thought they were lighthouses...

#+TODO: CHECKING FETCHING | UP_TO_DATE BEHIND MODIFIED MISSING ERROR"

  "Static header for the *scratch* dashboard.")

(defvar scratch-repos-file
  (expand-file-name "scratch-repos" user-emacs-directory)
  "File where `scratch-repos' is persisted.")

(defvar scratch-repos-extra-files nil
  "List of additional files to load repos from.
These files use the same format as `scratch-repos-file' but are
read-only from scratch's perspective — repos from these files are
not written back to `scratch-repos-file' on save.")

(defvar scratch-repos '(("~/.emacs.d"))
  "Alist of (PATH . REMOTE-URL) for git repositories to monitor.
Each entry is a cons cell where PATH is the local directory and
REMOTE-URL is the git remote URL (or nil if unknown).")

(defvar scratch--extra-repo-paths nil
  "Set of abbreviated paths loaded from extra files.
Used to exclude them when saving to the primary file.")

(defun scratch--repo-path (entry)
  "Return the local path from a `scratch-repos' ENTRY."
  (if (consp entry) (car entry) entry))

(defun scratch--repo-remote (entry)
  "Return the remote URL from a `scratch-repos' ENTRY, or nil."
  (when (consp entry) (cdr entry)))

(defvar scratch-modified-files-limit 10
  "Maximum number of modified files to display per repository.")

(defun scratch--write-repos-file (file repos)
  "Write REPOS alist to FILE in the scratch-repos format."
  (with-temp-file file
    (insert ";; -*- lexical-binding: t; -*-\n")
    (insert ";; Monitored git repositories for scratch dashboard.\n")
    (insert ";; This file is auto-generated. Edit via + in *scratch*.\n\n")
    (pp `(setq scratch-repos ',repos) (current-buffer))))

(defun scratch--save-repos ()
  "Write `scratch-repos' to `scratch-repos-file', excluding extra repos."
  (let ((primary (seq-remove
                  (lambda (e)
                    (member (scratch--abbrev-path (scratch--repo-path e))
                            scratch--extra-repo-paths))
                  scratch-repos)))
    (scratch--write-repos-file scratch-repos-file primary)))

(defun scratch--read-repos-from-file (file)
  "Read the repos alist from FILE without side effects on `scratch-repos'."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (repos)
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (when (and (listp form)
                           (eq (car form) 'setq)
                           (eq (cadr form) 'scratch-repos))
                  (setq repos (eval (caddr form) t)))))
          (end-of-file nil))
        repos))))

(defun scratch--load-repos ()
  "Load `scratch-repos' from `scratch-repos-file' and extra files."
  (when (file-exists-p scratch-repos-file)
    (load scratch-repos-file nil t t))
  (setq scratch--extra-repo-paths nil)
  (dolist (file scratch-repos-extra-files)
    (let ((extra (scratch--read-repos-from-file file)))
      (dolist (e extra)
        (push (scratch--abbrev-path (scratch--repo-path e))
              scratch--extra-repo-paths))
      (setq scratch-repos (append scratch-repos extra)))))

(defvar scratch--repo-statuses (make-hash-table :test 'equal)
  "Hash table mapping repo path to its status plist.
Each plist has keys: :branch :local :behind :error :state
:state is one of: checking, fetching, ready, error")

(defun scratch--abbrev-path (path)
  "Abbreviate PATH for display, replacing home dir with ~."
  (abbreviate-file-name (expand-file-name path)))

(defun scratch--repo-default-directory (repo)
  "Return the expanded default-directory for REPO."
  (file-name-as-directory (expand-file-name repo)))

(defun scratch--format-repo-entry (path status)
  "Return the org text for a single repo entry at PATH with STATUS plist."
  (let* ((state (plist-get status :state))
         (local-status (plist-get status :local))
         (behind (plist-get status :behind))
         (err (plist-get status :error))
         (dirty (not (null local-status)))
         (todo-kw (cond
                   ((eq state 'missing) "MISSING")
                   ((or (null state) (eq state 'checking)) "CHECKING")
                   ((eq state 'fetching) "FETCHING")
                   ((eq state 'error) "ERROR")
                   ((and behind (> behind 0)) "BEHIND")
                   (dirty "MODIFIED")
                   (t "UP_TO_DATE"))))
    (concat
     (format "** %s %s\n" todo-kw path)
     (cond
      ((eq state 'missing)
       "   - Directory not found. Press =c= to clone.\n")
      ((eq state 'ready)
       (concat
        (when (and behind (> behind 0))
          (format "   - %d commit%s behind upstream\n"
                  behind (if (> behind 1) "s" "")))
        (when dirty
          (format "   - Uncommitted: %s\n"
                  (replace-regexp-in-string "\n- " ", " local-status)))))
      ((eq state 'error)
       (format "   %s\n" (or err "unknown")))))))

(defun scratch--render ()
  "Render the git status dashboard in the *scratch* buffer."
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (with-current-buffer buf
        (unless (derived-mode-p 'scratch-dashboard-mode)
          (scratch-dashboard-mode))
        (let ((inhibit-read-only t)
              (point-was (point)))
          (erase-buffer)
          (insert scratch--header)
          (insert "\n\n* Repository Status\n\n")
          (dolist (entry scratch-repos)
            (let* ((repo (scratch--repo-path entry))
                   (path (scratch--abbrev-path repo))
                   (status (gethash path scratch--repo-statuses)))
              (insert (scratch--format-repo-entry path status))))
          ;; Simulate C-c C-c on #+TODO line to refresh local setup
          (goto-char (point-min))
          (re-search-forward "^#\\+")
          (org-ctrl-c-ctrl-c)
          (goto-char (min point-was (point-max))))))))

(defun scratch--update-repo (path)
  "Update the heading for PATH in-place in the *scratch* buffer."
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (status (gethash path scratch--repo-statuses))
              (new-text (scratch--format-repo-entry
                         path (gethash path scratch--repo-statuses))))
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward
                 (format "^\\*\\* [A-Z_]+ %s$" (regexp-quote path)) nil t)
                (let ((heading-start (line-beginning-position))
                      (heading-end (or (save-excursion
                                         (when (re-search-forward "^\\*\\* " nil t)
                                           (line-beginning-position)))
                                       (point-max))))
                  (delete-region heading-start heading-end)
                  (goto-char heading-start)
                  (insert new-text))
              ;; Heading not found — append before end of buffer
              (goto-char (point-max))
              (insert new-text))))))))

(defun scratch--repo-entry (repo)
  "Find the `scratch-repos' entry for REPO path."
  (seq-find (lambda (e) (equal (scratch--abbrev-path (scratch--repo-path e))
                               (scratch--abbrev-path repo)))
            scratch-repos))

(defun scratch--fetch-repo (repo)
  "Asynchronously fetch and gather status for REPO."
  (let* ((path (scratch--abbrev-path repo))
         (default-directory (scratch--repo-default-directory repo)))
    (if (not (file-directory-p default-directory))
        (let* ((entry (scratch--repo-entry repo))
               (remote (scratch--repo-remote entry)))
          (puthash path (list :state (if remote 'missing 'error)
                              :error (unless remote "Directory not found (no remote configured)"))
                   scratch--repo-statuses)
          (scratch--update-repo path))
      (if (not (file-directory-p (expand-file-name ".git" default-directory)))
          (progn
            (puthash path (list :state 'error :error "Not a git repo") scratch--repo-statuses)
            (scratch--update-repo path))
        (puthash path (list :state 'fetching) scratch--repo-statuses)
        (scratch--update-repo path)
        (let ((proc (start-process "scratch-git-fetch" nil "git" "fetch" "--quiet")))
          (set-process-sentinel
           proc
           (lambda (process _event)
             (if (not (eq (process-exit-status process) 0))
                 (progn
                   (puthash path (list :state 'error :error "Fetch failed") scratch--repo-statuses)
                   (scratch--update-repo path))
               (scratch--gather-status repo)))))))))

(defun scratch--gather-status (repo)
  "Gather branch, local changes, and behind count for REPO.
Called after a successful git fetch."
  (let* ((path (scratch--abbrev-path repo))
         (default-directory (scratch--repo-default-directory repo))
         (result (list :state 'ready :branch nil :local nil :behind 0 :error nil :files nil))
         (pending 3)
         (done-fn (lambda ()
                    (setq pending (1- pending))
                    (when (= pending 0)
                      (puthash path result scratch--repo-statuses)
                      (scratch--update-repo path)))))
    ;; Get current branch
    (let ((buf (generate-new-buffer " *scratch-branch*")))
      (set-process-sentinel
       (start-process "scratch-git-branch" buf "git" "rev-parse" "--abbrev-ref" "HEAD")
       (lambda (process _event)
         (when (eq (process-exit-status process) 0)
           (with-current-buffer (process-buffer process)
             (plist-put result :branch (string-trim (buffer-string)))))
         (kill-buffer (process-buffer process))
         (funcall done-fn))))
    ;; Get local status
    (let ((buf (generate-new-buffer " *scratch-porcelain*")))
      (set-process-sentinel
       (start-process "scratch-git-status" buf "git" "status" "--porcelain")
       (lambda (process _event)
         (when (eq (process-exit-status process) 0)
           (with-current-buffer (process-buffer process)
             (let* ((lines (split-string (buffer-string) "\n" t))
                    (modified 0)
                    (untracked 0)
                    (files nil))
               (dolist (line lines)
                 (if (string-prefix-p "?" line)
                     (setq untracked (1+ untracked))
                   (setq modified (1+ modified)))
                 (when (>= (length line) 3)
                   (push (substring line 3) files)))
               (plist-put result :files (nreverse files))
               (let ((parts nil))
                 (when (> untracked 0)
                   (push (format "Untracked %d files" untracked (if (> modified 1) "s" "")) parts))
                 (when (> modified 0)
                   (push (format "Modified %d file%s" modified (if (> modified 1) "s" "")) parts))
                 (when parts
                   (plist-put result :local (string-join parts "\n- ")))))))
         (kill-buffer (process-buffer process))
         (funcall done-fn))))
    ;; Get behind count
    (let ((buf (generate-new-buffer " *scratch-behind*")))
      (set-process-sentinel
       (start-process "scratch-git-behind" buf "git" "rev-list" "--count" "HEAD..@{u}")
       (lambda (process _event)
         (if (eq (process-exit-status process) 0)
             (with-current-buffer (process-buffer process)
               (let ((count (string-to-number (string-trim (buffer-string)))))
                 (plist-put result :behind count)))
           ;; No upstream configured — not an error, just "up to date"
           (plist-put result :behind 0))
         (kill-buffer (process-buffer process))
         (funcall done-fn))))))

(defun scratch--heading-repo-path ()
  "Return the repo path from the current org headline, or nil if not on one."
  (when-let ((heading (org-get-heading t t t t)))
    (when (string-match "\\` *\\(.*?\\)\\(?: on \\|$\\)" heading)
      (string-trim (match-string 1 heading)))))

(defun scratch--repo-path-at-point ()
  "Return the repo path at point, or nil if not on a repo headline."
  (save-excursion
    (when (and (derived-mode-p 'org-mode)
               (ignore-errors (org-back-to-heading t) t))
      (scratch--heading-repo-path))))

(defun scratch-search (str)
  "Search for STR in the buffer starting from the beginning."
  (interactive "sSearch: ")
  (goto-char (point-min))
  (unless (search-forward str nil t)
    (message "Not found: %s" str)))

(defun scratch-open-repo ()
  "Open magit-status for the repository on the current headline."
  (interactive)
  (let ((path (scratch--heading-repo-path)))
    (unless path
      (user-error "Not on a repo headline"))
    (let ((default-directory (scratch--repo-default-directory path)))
      (magit-status))))

(defun scratch--detect-remote (dir)
  "Detect the origin remote URL for the git repo at DIR."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (when (file-directory-p (expand-file-name ".git" default-directory))
      (string-trim
       (shell-command-to-string "git remote get-url origin 2>/dev/null")))))

(defun scratch--find-git-repos (dir)
  "Recursively find all git repositories under DIR.
Returns a list of absolute directory paths containing a .git directory.
Does not descend into .git directories or into nested repos."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
        repos)
    (if (file-directory-p (expand-file-name ".git" dir))
        (list dir)
      (dolist (entry (directory-files dir t "\\`[^.]" t))
        (when (and (file-directory-p entry)
                   (not (member (file-name-nondirectory entry) '(".git" "node_modules" ".cache"))))
          (setq repos (nconc repos (scratch--find-git-repos entry)))))
      repos)))

(defun scratch--add-single-repo (dir)
  "Add DIR as a monitored repo. Returns non-nil if added, nil if already present."
  (let ((path (scratch--abbrev-path dir)))
    (unless (seq-find (lambda (e) (equal (scratch--abbrev-path (scratch--repo-path e)) path))
                      scratch-repos)
      (let ((remote (scratch--detect-remote dir)))
        (setq scratch-repos
              (append scratch-repos
                      (list (cons path (if (string-empty-p remote) nil remote)))))
        (scratch--fetch-repo path)
        path))))

(defun scratch--choose-repos-file ()
  "Prompt the user to choose a repos file to save to.
Returns `scratch-repos-file' if no extra files are configured,
otherwise asks the user to pick from all available files."
  (if (null scratch-repos-extra-files)
      scratch-repos-file
    (let* ((all-files (cons scratch-repos-file scratch-repos-extra-files))
           (choices (mapcar #'abbreviate-file-name all-files))
           (choice (completing-read "Save to repos file: " choices nil t)))
      (expand-file-name choice))))

(defun scratch--append-to-repos-file (file new-entries)
  "Append NEW-ENTRIES to the repos already stored in FILE."
  (let* ((existing (scratch--read-repos-from-file file))
         (merged (append existing new-entries)))
    (scratch--write-repos-file file merged)
    ;; Update extra-repo-paths if this is an extra file.
    (unless (equal file scratch-repos-file)
      (dolist (e new-entries)
        (push (scratch--abbrev-path (scratch--repo-path e))
              scratch--extra-repo-paths)))))

(defun scratch-add-repo (dir)
  "Add DIR to monitored repositories and persist.
If DIR is a git repository, add it directly.
Otherwise, recursively find all git repos under DIR and add them.
When `scratch-repos-extra-files' is set, prompts for which file to save to."
  (interactive "DDirectory: ")
  (let* ((repos (scratch--find-git-repos dir))
         (added (cl-remove nil (mapcar #'scratch--add-single-repo repos))))
    (unless repos
      (user-error "No git repositories found in %s" (scratch--abbrev-path dir)))
    (when added
      (let* ((target (scratch--choose-repos-file))
             (new-entries (mapcar (lambda (path)
                                   (assoc path scratch-repos))
                                 added)))
        (if (equal target scratch-repos-file)
            (scratch--save-repos)
          (scratch--append-to-repos-file target new-entries)))
      (message "Added %d repo%s" (length added) (if (= 1 (length added)) "" "s")))))

(defun scratch-clone-repo ()
  "Clone the repository at point if it is missing locally."
  (interactive)
  (let* ((path (scratch--heading-repo-path))
         (_ (unless path (user-error "Not on a repo headline")))
         (status (gethash path scratch--repo-statuses))
         (_ (unless (eq (plist-get status :state) 'missing)
              (user-error "Repository is not missing")))
         (entry (scratch--repo-entry path))
         (remote (scratch--repo-remote entry))
         (_ (unless remote (user-error "No remote URL configured for %s" path)))
         (target (expand-file-name path)))
    (when (y-or-n-p (format "Clone %s to %s? " remote target))
      (puthash path (list :state 'checking) scratch--repo-statuses)
      (scratch--update-repo path)
      (let ((proc (start-process "scratch-git-clone" "*scratch-clone*"
                                 "git" "clone" remote target)))
        (set-process-sentinel
         proc
         (lambda (process _event)
           (if (eq (process-exit-status process) 0)
               (progn
                 (message "Cloned %s" path)
                 (scratch--fetch-repo path))
             (puthash path (list :state 'error :error "Clone failed")
                      scratch--repo-statuses)
             (scratch--update-repo path))))))))

(defun scratch-refresh ()
  "Refresh git status for the repo at point, or all repos if before first headline.
When point is on or inside a level-2 headline, refresh only that repo.
Otherwise refresh all monitored repositories."
  (interactive)
  (let ((path (scratch--repo-path-at-point)))
    (if path
        ;; Refresh single repo
        (progn
          (puthash path (list :state 'checking) scratch--repo-statuses)
          (scratch--update-repo path)
          (scratch--fetch-repo path))
      ;; Refresh all — full re-render
      (clrhash scratch--repo-statuses)
      (dolist (entry scratch-repos)
        (puthash (scratch--abbrev-path (scratch--repo-path entry))
                 (list :state 'checking)
                 scratch--repo-statuses))
      (scratch--render)
      (dolist (entry scratch-repos)
        (scratch--fetch-repo (scratch--repo-path entry))))))

(defun scratch--pull-repo (repo)
  "Asynchronously pull changes for REPO."
  (let* ((path (scratch--abbrev-path repo))
         (default-directory (scratch--repo-default-directory repo)))
    (if (not (file-directory-p (expand-file-name ".git" default-directory)))
        (progn
          (puthash path (list :state 'error :error "Not a git repo") scratch--repo-statuses)
          (scratch--update-repo path))
      (let ((proc (start-process "scratch-git-pull" nil "git" "pull" "--quiet")))
        (set-process-sentinel
         proc
         (lambda (process _event)
           (if (not (eq (process-exit-status process) 0))
               (progn
                 (puthash path (list :state 'error :error "Pull failed") scratch--repo-statuses)
                 (scratch--update-repo path))
             (scratch--gather-status repo))))))))

(defun scratch-pull-repo ()
  "Pull changes for the repository at point."
  (interactive)
  (let ((path (scratch--repo-path-at-point)))
    (unless path
      (user-error "Not on a repo headline"))
    (puthash path (list :state 'fetching) scratch--repo-statuses)
    (scratch--update-repo path)
    (scratch--pull-repo path)))

(defun scratch-pull-all ()
  "Pull changes for all monitored repositories."
  (interactive)
  (dolist (entry scratch-repos)
    (let ((path (scratch--abbrev-path (scratch--repo-path entry))))
      (puthash path (list :state 'fetching) scratch--repo-statuses)
      (scratch--update-repo path)))
  (dolist (entry scratch-repos)
    (scratch--pull-repo (scratch--repo-path entry))))

(defun scratch-refresh-all ()
  "Refresh git status for all monitored repositories."
  (interactive)
  (clrhash scratch--repo-statuses)
  (dolist (entry scratch-repos)
    (puthash (scratch--abbrev-path (scratch--repo-path entry))
             (list :state 'checking)
             scratch--repo-statuses))
  (scratch--render)
  (dolist (entry scratch-repos)
    (scratch--fetch-repo (scratch--repo-path entry))))

(defun scratch--migrate-repos ()
  "Migrate old flat list format to alist format if needed."
  (when (and scratch-repos (stringp (car scratch-repos)))
    (setq scratch-repos
          (mapcar (lambda (path) (cons path nil)) scratch-repos))
    (scratch--save-repos)))

(defun scratch--startup ()
  "Initialize the scratch buffer dashboard on startup."
  (scratch--load-repos)
  (scratch--migrate-repos)
  (scratch-refresh))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'scratch--startup)
  (add-hook 'emacs-startup-hook #'scratch--startup))

(provide 'mijn-scratch)
