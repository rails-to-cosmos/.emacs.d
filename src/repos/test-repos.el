;;; test-repos.el --- Tests for repos.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'repos)

;; ---------------------------------------------------------------------------
;; Helper
;; ---------------------------------------------------------------------------

(defmacro with-repos-buffer (&rest body)
  "Run BODY with a temporary *repos* buffer and clean state."
  (declare (indent 0))
  `(let ((repos-list repos-list)
         (repos--statuses (make-hash-table :test 'equal))
         (repos-dashboard-buffer-name "*repos-test*")
         (repos--current-sort 'status)
         (repos--sort-ascending t))
     (unwind-protect
         (progn
           (get-buffer-create repos-dashboard-buffer-name)
           ,@body)
       (when (get-buffer repos-dashboard-buffer-name)
         (kill-buffer repos-dashboard-buffer-name)))))

;; ---------------------------------------------------------------------------
;; repos-dashboard-mode
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-mode-derives-from-org ()
  "repos-dashboard-mode should derive from org-mode."
  (with-temp-buffer
    (repos-dashboard-mode)
    (should (derived-mode-p 'org-mode))))

(ert-deftest test-repos-mode-is-read-only ()
  "repos-dashboard-mode should set buffer-read-only."
  (with-temp-buffer
    (repos-dashboard-mode)
    (should buffer-read-only)))

;; ---------------------------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-keybindings ()
  "Key bindings should be set in repos-dashboard-mode-map."
  (should (eq (lookup-key repos-dashboard-mode-map "g") #'repos-refresh))
  (should (eq (lookup-key repos-dashboard-mode-map "G") #'repos-refresh-all))
  (should (eq (lookup-key repos-dashboard-mode-map "+") #'repos-add-repo))
  (should (eq (lookup-key repos-dashboard-mode-map "c") #'repos-clone-repo))
  (should (eq (lookup-key repos-dashboard-mode-map "C") #'repos-clone-all-missing))
  (should (eq (lookup-key repos-dashboard-mode-map "f") #'repos-pull-repo))
  (should (eq (lookup-key repos-dashboard-mode-map "F") #'repos-pull-all))
  (should (eq (lookup-key repos-dashboard-mode-map "^") #'repos-cycle-sort))
  (should (eq (lookup-key repos-dashboard-mode-map "~") #'repos-toggle-sort-direction))
  (should (eq (lookup-key repos-dashboard-mode-map "q") #'quit-window)))

;; ---------------------------------------------------------------------------
;; Accessors
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-path-from-cons ()
  "Should return car of cons entry."
  (should (equal "~/foo" (repos--path '("~/foo" . "git@host:repo")))))

(ert-deftest test-repos-path-from-string ()
  "Should return the string itself for legacy entries."
  (should (equal "~/foo" (repos--path "~/foo"))))

(ert-deftest test-repos-remote-from-cons ()
  "Should return cdr of cons entry."
  (should (equal "git@host:repo" (repos--remote '("~/foo" . "git@host:repo")))))

(ert-deftest test-repos-remote-nil-when-missing ()
  "Should return nil when no remote is set."
  (should (null (repos--remote '("~/foo")))))

(ert-deftest test-repos-abbrev-home ()
  "Should abbreviate home directory to ~."
  (let ((home (expand-file-name "~")))
    (should (string-prefix-p "~/" (repos--abbrev (concat home "/foo"))))))

(ert-deftest test-repos-default-directory-trailing-slash ()
  "Should return a path ending with /."
  (should (string-suffix-p "/" (repos--default-directory "~/.emacs.d"))))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-render-checking ()
  "Should show CHECKING for repos in checking state."
  (with-repos-buffer
    (setq repos-list '(("~/.emacs.d")))
    (puthash (repos--abbrev "~/.emacs.d")
             (list :state 'checking) repos--statuses)
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (should (string-match-p "\\*\\* CHECKING" (buffer-string))))))

(ert-deftest test-repos-render-error ()
  "Should show ERROR and error message."
  (with-repos-buffer
    (setq repos-list '(("~/.emacs.d")))
    (puthash (repos--abbrev "~/.emacs.d")
             (list :state 'error :error "Fetch failed") repos--statuses)
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* ERROR" text))
        (should (string-match-p "Fetch failed" text))))))

(ert-deftest test-repos-render-up-to-date ()
  "Should show UP_TO_DATE for clean repos."
  (with-repos-buffer
    (setq repos-list '(("~/.emacs.d")))
    (puthash (repos--abbrev "~/.emacs.d")
             (list :state 'ready :behind 0 :modified 0 :untracked 0) repos--statuses)
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (should (string-match-p "\\*\\* UP_TO_DATE" (buffer-string))))))

(ert-deftest test-repos-render-behind ()
  "Should show BEHIND when commits behind upstream."
  (with-repos-buffer
    (setq repos-list '(("~/.emacs.d")))
    (puthash (repos--abbrev "~/.emacs.d")
             (list :state 'ready :behind 3 :modified 0 :untracked 0) repos--statuses)
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (should (string-match-p "\\*\\* BEHIND" (buffer-string))))))

(ert-deftest test-repos-render-modified ()
  "Should show MODIFIED when files are changed."
  (with-repos-buffer
    (setq repos-list '(("~/.emacs.d")))
    (puthash (repos--abbrev "~/.emacs.d")
             (list :state 'ready :behind 0 :modified 2 :untracked 0
                   :local "Modified 2 files") repos--statuses)
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (should (string-match-p "\\*\\* MODIFIED" (buffer-string))))))

(ert-deftest test-repos-render-untracked ()
  "Should show UNTRACKED when only untracked files exist."
  (with-repos-buffer
    (setq repos-list '(("~/.emacs.d")))
    (puthash (repos--abbrev "~/.emacs.d")
             (list :state 'ready :behind 0 :modified 0 :untracked 3
                   :local "Untracked 3 files") repos--statuses)
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (should (string-match-p "\\*\\* UNTRACKED" (buffer-string))))))

(ert-deftest test-repos-render-missing ()
  "Should show MISSING and clone hint."
  (with-repos-buffer
    (setq repos-list '(("~/nonexistent" . "git@host:repo")))
    (puthash (repos--abbrev "~/nonexistent")
             (list :state 'missing) repos--statuses)
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* MISSING" text))
        (should (string-match-p "clone" text))))))

(ert-deftest test-repos-render-idempotent ()
  "Multiple renders should not duplicate content."
  (with-repos-buffer
    (setq repos-list '(("~/.emacs.d")))
    (puthash (repos--abbrev "~/.emacs.d")
             (list :state 'checking) repos--statuses)
    (repos--render)
    (puthash (repos--abbrev "~/.emacs.d")
             (list :state 'ready :behind 0 :modified 0 :untracked 0) repos--statuses)
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (let ((text (buffer-string)))
        (should (= 1 (with-temp-buffer
                        (insert text)
                        (goto-char (point-min))
                        (let ((count 0))
                          (while (re-search-forward "^\\* Repository Status" nil t)
                            (setq count (1+ count)))
                          count))))))))

(ert-deftest test-repos-render-multiple ()
  "Should render all repos."
  (with-repos-buffer
    (setq repos-list '(("~/.emacs.d") ("/tmp")))
    (puthash (repos--abbrev "~/.emacs.d")
             (list :state 'ready :behind 0 :modified 0 :untracked 0) repos--statuses)
    (puthash (repos--abbrev "/tmp")
             (list :state 'error :error "Not a git repo") repos--statuses)
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "UP_TO_DATE.*~/.emacs.d" text))
        (should (string-match-p "ERROR.*/tmp" text))))))

(ert-deftest test-repos-render-shows-sort ()
  "Heading should show current sort method."
  (with-repos-buffer
    (setq repos-list '(("~/.emacs.d")))
    (repos--render)
    (with-current-buffer repos-dashboard-buffer-name
      (should (string-match-p "sort: status asc" (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Sorting
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-sort-by-status ()
  "Status sort should put errors before up-to-date."
  (with-repos-buffer
    (setq repos-list '(("~/a") ("~/b")))
    (puthash (repos--abbrev "~/a")
             (list :state 'ready :behind 0 :modified 0 :untracked 0) repos--statuses)
    (puthash (repos--abbrev "~/b")
             (list :state 'error :error "fail") repos--statuses)
    (let ((sorted (repos--sorted-entries)))
      (should (equal "~/b" (repos--path (car sorted)))))))

(ert-deftest test-repos-sort-by-name ()
  "Name sort should alphabetize by directory name."
  (with-repos-buffer
    (setq repos-list '(("~/zebra") ("~/alpha")))
    (setq repos--current-sort 'name)
    (let ((sorted (repos--sorted-entries)))
      (should (equal "~/alpha" (repos--path (car sorted)))))))

(ert-deftest test-repos-sort-descending ()
  "Descending should reverse the order."
  (with-repos-buffer
    (setq repos-list '(("~/zebra") ("~/alpha")))
    (setq repos--current-sort 'name)
    (setq repos--sort-ascending nil)
    (let ((sorted (repos--sorted-entries)))
      (should (equal "~/zebra" (repos--path (car sorted)))))))

(ert-deftest test-repos-todo-order ()
  "Status priority should follow #+TODO header order."
  (with-repos-buffer
    (puthash "checking" (list :state 'checking) repos--statuses)
    (puthash "error" (list :state 'error) repos--statuses)
    (puthash "ok" (list :state 'ready :behind 0 :modified 0 :untracked 0) repos--statuses)
    (should (< (repos--status-priority "checking")
               (repos--status-priority "error")))
    (should (< (repos--status-priority "error")
               (repos--status-priority "ok")))))

;; ---------------------------------------------------------------------------
;; Persistence
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-write-and-read ()
  "Should round-trip repos through a file."
  (let ((file (make-temp-file "repos-test"))
        (data '(("~/.emacs.d" . "git@github.com:user/repo.git")
                ("/tmp/foo"))))
    (unwind-protect
        (progn
          (repos--write-file file data)
          (let ((result (repos--read-from-file file)))
            (should (equal result data))))
      (delete-file file))))

(ert-deftest test-repos-read-legacy-format ()
  "Should read files that use the old scratch-repos variable."
  (let ((file (make-temp-file "repos-legacy")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(setq scratch-repos '((\"~/old\" . \"git@host:repo\")))\n"))
          (let ((result (repos--read-from-file file)))
            (should (equal result '(("~/old" . "git@host:repo"))))))
      (delete-file file))))

(ert-deftest test-repos-read-missing-file ()
  "Should return nil for nonexistent file."
  (should (null (repos--read-from-file "/tmp/nonexistent-repos-file"))))

;; ---------------------------------------------------------------------------
;; Backward compatibility
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-scratch-alias ()
  "scratch-repos should alias repos-list."
  (let ((repos-list '(("~/test"))))
    (should (equal scratch-repos '(("~/test"))))))

(provide 'test-repos)

;;; test-repos.el ends here
