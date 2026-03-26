;;; repos.el --- Git repository dashboard -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ol)
(require 'magit)

(declare-function magit-status "magit-status")

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
  "#+TODO: CHECKING FETCHING BEHIND MODIFIED MISSING ERROR | UP_TO_DATE UNTRACKED"
  "Static header for the dashboard.")

(defvar repos-file
  (expand-file-name "scratch-repos" user-emacs-directory)
  "File where `repos-list' is persisted.")

(defvar repos-extra-files nil
  "List of additional files to load repos from.
Read-only — repos from these files are not written back on save.")

(defvar repos-list '(("~/.emacs.d"))
  "Alist of (PATH . REMOTE-URL) for git repositories to monitor.")

(defvar repos--extra-paths nil
  "Set of abbreviated paths loaded from extra files.")

;;; Backward compatibility — scratch-repos files set this variable
(defvaralias 'scratch-repos 'repos-list)
(defvaralias 'scratch-repos-file 'repos-file)
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

(defun repos--default-directory (repo)
  "Return the expanded default-directory for REPO."
  (file-name-as-directory (expand-file-name repo)))

;;; Persistence

(defun repos--write-file (file repos)
  "Write REPOS alist to FILE."
  (with-temp-file file
    (insert ";; -*- lexical-binding: t; -*-\n")
    (insert ";; Monitored git repositories for scratch dashboard.\n")
    (insert ";; This file is auto-generated. Edit via + in *repos*.\n\n")
    (pp `(setq scratch-repos ',repos) (current-buffer))))

(defun repos--save ()
  "Write repos to file, excluding extra repos."
  (let ((primary (seq-remove
                  (lambda (e)
                    (member (repos--abbrev (repos--path e))
                            repos--extra-paths))
                  repos-list)))
    (repos--write-file repos-file primary)))

(defun repos--read-from-file (file)
  "Read the repos alist from FILE without side effects."
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

(defun repos--load ()
  "Load repos from primary file and extra files."
  (when (file-exists-p repos-file)
    (load repos-file nil t t))
  (setq repos--extra-paths nil)
  (dolist (file repos-extra-files)
    (let ((extra (repos--read-from-file file)))
      (dolist (e extra)
        (push (repos--abbrev (repos--path e)) repos--extra-paths))
      (setq repos-list (append repos-list extra)))))

;;; Status Tracking

(defvar repos--statuses (make-hash-table :test 'equal)
  "Hash table mapping repo path to its status plist.")

;;; Sorting

(defvar repos--sort-methods '(status name path)
  "Available sort methods for the dashboard.")

(defvar repos--current-sort 'status
  "Current sort method for the dashboard.")

(defvar repos--sort-ascending t
  "Non-nil for ascending sort, nil for descending.")

(defconst repos--todo-order
  '("CHECKING" "FETCHING" "BEHIND" "MODIFIED" "MISSING" "ERROR" "UP_TO_DATE" "UNTRACKED")
  "Status keywords in #+TODO header order, used for sorting.")

(defun repos--todo-kw-for-path (path)
  "Return the TODO keyword string for the repo at PATH."
  (let* ((status (gethash path repos--statuses))
         (state (plist-get status :state))
         (behind (or (plist-get status :behind) 0))
         (mod (or (plist-get status :modified) 0))
         (untracked (or (plist-get status :untracked) 0)))
    (cond
     ((eq state 'missing) "MISSING")
     ((or (null state) (eq state 'checking)) "CHECKING")
     ((eq state 'fetching) "FETCHING")
     ((eq state 'error) "ERROR")
     ((> behind 0) "BEHIND")
     ((> mod 0) "MODIFIED")
     ((> untracked 0) "UNTRACKED")
     (t "UP_TO_DATE"))))

(defun repos--status-priority (path)
  "Return a numeric priority for the repo at PATH based on #+TODO order."
  (let ((kw (repos--todo-kw-for-path path)))
    (or (cl-position kw repos--todo-order :test #'equal)
        (length repos--todo-order))))

(defun repos--sorted-entries ()
  "Return `repos-list' sorted by the current method and direction."
  (let* ((entries (copy-sequence repos-list))
         (sorted
          (pcase repos--current-sort
            ('name
             (sort entries (lambda (a b)
                             (string< (file-name-nondirectory
                                       (directory-file-name (repos--path a)))
                                      (file-name-nondirectory
                                       (directory-file-name (repos--path b)))))))
            ('path
             (sort entries (lambda (a b) (string< (repos--path a) (repos--path b)))))
            ('status
             (sort entries (lambda (a b)
                             (< (repos--status-priority (repos--abbrev (repos--path a)))
                                (repos--status-priority (repos--abbrev (repos--path b)))))))
            (_ entries))))
    (if repos--sort-ascending sorted (nreverse sorted))))

(defun repos--sort-label ()
  "Return a string describing the current sort."
  (format "%s %s" repos--current-sort (if repos--sort-ascending "asc" "desc")))

;;;###autoload
(defun repos-cycle-sort ()
  "Cycle through sort methods. On the same method, toggle asc/desc."
  (interactive)
  (let* ((idx (cl-position repos--current-sort repos--sort-methods))
         (next (nth (mod (1+ (or idx 0)) (length repos--sort-methods))
                    repos--sort-methods)))
    (if (eq next repos--current-sort)
        (setq repos--sort-ascending (not repos--sort-ascending))
      (setq repos--current-sort next
            repos--sort-ascending t))
    (repos--render)
    (message "Sort: %s" (repos--sort-label))))

;;;###autoload
(defun repos-toggle-sort-direction ()
  "Toggle ascending/descending for the current sort method."
  (interactive)
  (setq repos--sort-ascending (not repos--sort-ascending))
  (repos--render)
  (message "Sort: %s" (repos--sort-label)))

;;; Rendering

(defun repos--format-entry (path status)
  "Return the org text for a single repo at PATH with STATUS."
  (let* ((state (plist-get status :state))
         (local-status (plist-get status :local))
         (behind (plist-get status :behind))
         (err (plist-get status :error))
         (mod-count (or (plist-get status :modified) 0))
         (untracked-count (or (plist-get status :untracked) 0))
         (dirty (not (null local-status)))
         (todo-kw (cond
                   ((eq state 'missing) "MISSING")
                   ((or (null state) (eq state 'checking)) "CHECKING")
                   ((eq state 'fetching) "FETCHING")
                   ((eq state 'error) "ERROR")
                   ((and behind (> behind 0)) "BEHIND")
                   ((> mod-count 0) "MODIFIED")
                   ((> untracked-count 0) "UNTRACKED")
                   (t "UP_TO_DATE"))))
    (concat
     (format "** %s %s\n" todo-kw path)
     (cond
      ((eq state 'missing)
       "   - Directory not found. Press =c= to clone, =C= to clone all.\n")
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

(defun repos--render ()
  "Render the dashboard buffer."
  (let ((buf (get-buffer repos-dashboard-buffer-name)))
    (when buf
      (with-current-buffer buf
        (unless (derived-mode-p 'repos-dashboard-mode)
          (repos-dashboard-mode))
        (let ((inhibit-read-only t)
              (point-was (point)))
          (erase-buffer)
          (insert repos--header)
          (insert (format "\n\n* Repository Status [/]  (sort: %s)\n\n"
                          (repos--sort-label)))
          (dolist (entry (repos--sorted-entries))
            (let* ((repo (repos--path entry))
                   (path (repos--abbrev repo))
                   (status (gethash path repos--statuses)))
              (insert (repos--format-entry path status))))
          (goto-char (point-min))
          (re-search-forward "^#\\+")
          (org-ctrl-c-ctrl-c)
          (goto-char (min point-was (point-max))))))))

(defun repos--update (path)
  "Update the heading for PATH in-place."
  (let ((buf (get-buffer repos-dashboard-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (new-text (repos--format-entry
                         path (gethash path repos--statuses))))
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
              (goto-char (point-max))
              (insert new-text))
            (goto-char (point-min))
            (when (re-search-forward "^\\* Repository Status" nil t)
              (org-update-statistics-cookies nil))))))))

;;; Async Git Operations

(defun repos--entry (repo)
  "Find the repos entry for REPO path."
  (seq-find (lambda (e) (equal (repos--abbrev (repos--path e))
                               (repos--abbrev repo)))
            repos-list))

(defun repos--fetch (repo)
  "Asynchronously fetch and gather status for REPO."
  (let* ((path (repos--abbrev repo))
         (default-directory (repos--default-directory repo)))
    (if (not (file-directory-p default-directory))
        (let* ((entry (repos--entry repo))
               (remote (repos--remote entry)))
          (puthash path (list :state (if remote 'missing 'error)
                              :error (unless remote "Directory not found (no remote configured)"))
                   repos--statuses)
          (repos--update path))
      (if (not (file-directory-p (expand-file-name ".git" default-directory)))
          (progn
            (puthash path (list :state 'error :error "Not a git repo") repos--statuses)
            (repos--update path))
        (puthash path (list :state 'fetching) repos--statuses)
        (repos--update path)
        (let ((proc (start-process "repos-fetch" nil "git" "fetch" "--quiet")))
          (set-process-sentinel
           proc
           (lambda (process _event)
             (if (not (eq (process-exit-status process) 0))
                 (progn
                   (puthash path (list :state 'error :error "Fetch failed") repos--statuses)
                   (repos--update path))
               (repos--gather repo)))))))))

(defun repos--gather (repo)
  "Gather branch, local changes, and behind count for REPO."
  (let* ((path (repos--abbrev repo))
         (default-directory (repos--default-directory repo))
         (result (list :state 'ready :branch nil :local nil :behind 0 :error nil :files nil))
         (pending 3)
         (done-fn (lambda ()
                    (setq pending (1- pending))
                    (when (= pending 0)
                      (puthash path result repos--statuses)
                      (repos--update path)))))
    (let ((buf (generate-new-buffer " *repos-branch*")))
      (set-process-sentinel
       (start-process "repos-branch" buf "git" "rev-parse" "--abbrev-ref" "HEAD")
       (lambda (process _event)
         (when (eq (process-exit-status process) 0)
           (with-current-buffer (process-buffer process)
             (plist-put result :branch (string-trim (buffer-string)))))
         (kill-buffer (process-buffer process))
         (funcall done-fn))))
    (let ((buf (generate-new-buffer " *repos-porcelain*")))
      (set-process-sentinel
       (start-process "repos-status" buf "git" "status" "--porcelain")
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
               (plist-put result :modified modified)
               (plist-put result :untracked untracked)
               (let ((parts nil))
                 (when (> untracked 0)
                   (push (format "Untracked %d files" untracked) parts))
                 (when (> modified 0)
                   (push (format "Modified %d file%s" modified (if (> modified 1) "s" "")) parts))
                 (when parts
                   (plist-put result :local (string-join parts "\n- ")))))))
         (kill-buffer (process-buffer process))
         (funcall done-fn))))
    (let ((buf (generate-new-buffer " *repos-behind*")))
      (set-process-sentinel
       (start-process "repos-behind" buf "git" "rev-list" "--count" "HEAD..@{u}")
       (lambda (process _event)
         (if (eq (process-exit-status process) 0)
             (with-current-buffer (process-buffer process)
               (let ((count (string-to-number (string-trim (buffer-string)))))
                 (plist-put result :behind count)))
           (plist-put result :behind 0))
         (kill-buffer (process-buffer process))
         (funcall done-fn))))))

(defun repos--pull (repo)
  "Asynchronously pull changes for REPO."
  (let* ((path (repos--abbrev repo))
         (default-directory (repos--default-directory repo)))
    (if (not (file-directory-p (expand-file-name ".git" default-directory)))
        (progn
          (puthash path (list :state 'error :error "Not a git repo") repos--statuses)
          (repos--update path))
      (let ((proc (start-process "repos-pull" nil "git" "pull" "--quiet")))
        (set-process-sentinel
         proc
         (lambda (process _event)
           (if (not (eq (process-exit-status process) 0))
               (progn
                 (puthash path (list :state 'error :error "Pull failed") repos--statuses)
                 (repos--update path))
             (repos--gather repo))))))))

(defun repos--clone-async (path remote target)
  "Clone REMOTE into TARGET for repo at PATH."
  (puthash path (list :state 'checking) repos--statuses)
  (repos--update path)
  (let ((proc (start-process "repos-clone" "*repos-clone*"
                             "git" "clone" remote target)))
    (set-process-sentinel
     proc
     (lambda (process _event)
       (if (eq (process-exit-status process) 0)
           (progn
             (message "Cloned %s" path)
             (repos--fetch path))
         (puthash path (list :state 'error :error "Clone failed")
                  repos--statuses)
         (repos--update path))))))

;;; Helpers

(defun repos--heading-path ()
  "Return the repo path from the current org headline."
  (when-let ((heading (org-get-heading t t t t)))
    (when (string-match "\\` *\\(.*?\\)\\(?: on \\|$\\)" heading)
      (string-trim (match-string 1 heading)))))

(defun repos--path-at-point ()
  "Return the repo path at point."
  (save-excursion
    (when (and (derived-mode-p 'org-mode)
               (ignore-errors (org-back-to-heading t) t))
      (repos--heading-path))))

(defun repos--detect-remote (dir)
  "Detect the origin remote URL for DIR."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (when (file-directory-p (expand-file-name ".git" default-directory))
      (string-trim
       (shell-command-to-string "git remote get-url origin 2>/dev/null")))))

(defun repos--find-git-repos (dir)
  "Recursively find all git repositories under DIR."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
        result)
    (if (file-directory-p (expand-file-name ".git" dir))
        (list dir)
      (dolist (entry (directory-files dir t "\\`[^.]" t))
        (when (and (file-directory-p entry)
                   (not (member (file-name-nondirectory entry) '(".git" "node_modules" ".cache"))))
          (setq result (nconc result (repos--find-git-repos entry)))))
      result)))

(defun repos--choose-file ()
  "Prompt to choose a repos file to save to."
  (if (null repos-extra-files)
      repos-file
    (let* ((all (cons repos-file repos-extra-files))
           (choices (mapcar #'abbreviate-file-name all))
           (choice (completing-read "Save to repos file: " choices nil t)))
      (expand-file-name choice))))

(defun repos--append-to-file (file new-entries)
  "Append NEW-ENTRIES to the repos in FILE."
  (let* ((existing (repos--read-from-file file))
         (merged (append existing new-entries)))
    (repos--write-file file merged)
    (unless (equal file repos-file)
      (dolist (e new-entries)
        (push (repos--abbrev (repos--path e)) repos--extra-paths)))))

;;; Interactive Commands

;;;###autoload
(defun repos-search (str)
  "Search for STR in the dashboard."
  (interactive "sSearch: ")
  (goto-char (point-min))
  (unless (search-forward str nil t)
    (message "Not found: %s" str)))

;;;###autoload
(defun repos-open-repo ()
  "Open magit-status for the repository at point."
  (interactive)
  (let ((path (repos--heading-path)))
    (unless path (user-error "Not on a repo headline"))
    (let ((default-directory (repos--default-directory path)))
      (magit-status))))

;;;###autoload
(defun repos-add-repo (dir)
  "Add DIR to monitored repositories."
  (interactive "DDirectory: ")
  (let* ((found (repos--find-git-repos dir))
         (added (cl-remove nil (mapcar
                                (lambda (d)
                                  (let ((path (repos--abbrev d)))
                                    (unless (seq-find (lambda (e) (equal (repos--abbrev (repos--path e)) path))
                                                      repos-list)
                                      (let ((remote (repos--detect-remote d)))
                                        (setq repos-list
                                              (append repos-list
                                                      (list (cons path (if (string-empty-p remote) nil remote)))))
                                        (repos--fetch path)
                                        path))))
                                found))))
    (unless found
      (user-error "No git repositories found in %s" (repos--abbrev dir)))
    (when added
      (let* ((target (repos--choose-file))
             (new-entries (mapcar (lambda (path) (assoc path repos-list)) added)))
        (if (equal target repos-file)
            (repos--save)
          (repos--append-to-file target new-entries)))
      (message "Added %d repo%s" (length added) (if (= 1 (length added)) "" "s")))))

;;;###autoload
(defun repos-clone-repo ()
  "Clone the repository at point."
  (interactive)
  (let* ((path (repos--heading-path))
         (_ (unless path (user-error "Not on a repo headline")))
         (status (gethash path repos--statuses))
         (_ (unless (eq (plist-get status :state) 'missing)
              (user-error "Repository is not missing")))
         (entry (repos--entry path))
         (remote (repos--remote entry))
         (_ (unless remote (user-error "No remote URL configured for %s" path)))
         (target (expand-file-name path)))
    (when (y-or-n-p (format "Clone %s to %s? " remote target))
      (repos--clone-async path remote target))))

;;;###autoload
(defun repos-clone-all-missing ()
  "Clone all repositories in the missing state."
  (interactive)
  (let ((missing
         (cl-loop for entry in repos-list
                  for repo = (repos--path entry)
                  for path = (repos--abbrev repo)
                  for status = (gethash path repos--statuses)
                  for remote = (repos--remote entry)
                  when (and (eq (plist-get status :state) 'missing) remote)
                  collect (list path remote (expand-file-name path)))))
    (if (null missing)
        (message "No missing repositories to clone")
      (when (y-or-n-p (format "Clone %d missing repo%s? "
                               (length missing)
                               (if (= 1 (length missing)) "" "s")))
        (dolist (m missing)
          (repos--clone-async (nth 0 m) (nth 1 m) (nth 2 m)))))))

;;;###autoload
(defun repos-refresh ()
  "Refresh repo at point, or all repos."
  (interactive)
  (let ((path (repos--path-at-point)))
    (if path
        (progn
          (puthash path (list :state 'checking) repos--statuses)
          (repos--update path)
          (repos--fetch path))
      (clrhash repos--statuses)
      (dolist (entry repos-list)
        (puthash (repos--abbrev (repos--path entry))
                 (list :state 'checking)
                 repos--statuses))
      (repos--render)
      (dolist (entry repos-list)
        (repos--fetch (repos--path entry))))))

;;;###autoload
(defun repos-refresh-all ()
  "Refresh all monitored repositories."
  (interactive)
  (clrhash repos--statuses)
  (dolist (entry repos-list)
    (puthash (repos--abbrev (repos--path entry))
             (list :state 'checking)
             repos--statuses))
  (repos--render)
  (dolist (entry repos-list)
    (repos--fetch (repos--path entry))))

;;;###autoload
(defun repos-pull-repo ()
  "Pull changes for the repository at point."
  (interactive)
  (let ((path (repos--path-at-point)))
    (unless path (user-error "Not on a repo headline"))
    (puthash path (list :state 'fetching) repos--statuses)
    (repos--update path)
    (repos--pull path)))

;;;###autoload
(defun repos-pull-all ()
  "Pull changes for all monitored repositories."
  (interactive)
  (dolist (entry repos-list)
    (let ((path (repos--abbrev (repos--path entry))))
      (puthash path (list :state 'fetching) repos--statuses)
      (repos--update path)))
  (dolist (entry repos-list)
    (repos--pull (repos--path entry))))

;;; Migration & Startup

(defun repos--migrate ()
  "Migrate old flat list format to alist format if needed."
  (when (and repos-list (stringp (car repos-list)))
    (setq repos-list
          (mapcar (lambda (path) (cons path nil)) repos-list))
    (repos--save)))

(defun repos--startup ()
  "Load repos on startup."
  (repos--load)
  (repos--migrate))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'repos--startup)
  (add-hook 'emacs-startup-hook #'repos--startup))

;;; Entry Point

;;;###autoload
(defun repos-dashboard ()
  "Open the repository dashboard buffer."
  (interactive)
  (let ((buf (get-buffer-create repos-dashboard-buffer-name)))
    (pop-to-buffer buf)
    (unless (derived-mode-p 'repos-dashboard-mode)
      (repos-dashboard-mode))
    (repos-refresh)))

(global-set-key (kbd "C-x y p") #'repos-dashboard)

(provide 'repos)

;;; repos.el ends here
