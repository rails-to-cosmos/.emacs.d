;;; test-my-scratch.el --- Tests for init-scratch -*- lexical-binding: t; -*-

(require 'ert)
(require 'my-scratch)

;; ---------------------------------------------------------------------------
;; Helper
;; ---------------------------------------------------------------------------

(defmacro with-scratch-buffer (initial-content &rest body)
  "Run BODY with a temporary *scratch* buffer containing INITIAL-CONTENT."
  (declare (indent 1))
  `(let ((orig (get-buffer "*scratch*")))
     (when orig (kill-buffer orig))
     (with-current-buffer (get-buffer-create "*scratch*")
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert ,initial-content))
       (let ((scratch-repos scratch-repos)
             (scratch--repo-statuses (make-hash-table :test 'equal)))
         ,@body))))

;; ---------------------------------------------------------------------------
;; scratch-dashboard-mode
;; ---------------------------------------------------------------------------

(ert-deftest test-scratch-dashboard-mode-derives-from-org ()
  "scratch-dashboard-mode should be derived from org-mode."
  (with-temp-buffer
    (scratch-dashboard-mode)
    (should (derived-mode-p 'org-mode))))

(ert-deftest test-scratch-dashboard-mode-is-read-only ()
  "scratch-dashboard-mode should set buffer-read-only."
  (with-temp-buffer
    (scratch-dashboard-mode)
    (should buffer-read-only)))

(ert-deftest test-scratch-dashboard-mode-keybindings ()
  "Key bindings n/p/f/b/g/+ should be set in scratch-dashboard-mode."
  (should (eq (lookup-key scratch-dashboard-mode-map "n") #'org-next-visible-heading))
  (should (eq (lookup-key scratch-dashboard-mode-map "p") #'org-previous-visible-heading))
  (should (eq (lookup-key scratch-dashboard-mode-map "f") #'forward-char))
  (should (eq (lookup-key scratch-dashboard-mode-map "b") #'backward-char))
  (should (eq (lookup-key scratch-dashboard-mode-map "g") #'scratch-refresh))
  (should (eq (lookup-key scratch-dashboard-mode-map "+") #'scratch-add-repo)))

;; ---------------------------------------------------------------------------
;; scratch--abbrev-path
;; ---------------------------------------------------------------------------

(ert-deftest test-abbrev-path-home-dir ()
  "Should abbreviate home directory to ~."
  (let ((home (expand-file-name "~")))
    (should (string-prefix-p "~/" (scratch--abbrev-path (concat home "/foo"))))))

(ert-deftest test-abbrev-path-expands-tilde ()
  "Should expand ~ before abbreviating."
  (let ((result (scratch--abbrev-path "~/.emacs.d")))
    (should (string= result "~/.emacs.d"))))

;; ---------------------------------------------------------------------------
;; scratch--repo-default-directory
;; ---------------------------------------------------------------------------

(ert-deftest test-repo-default-directory-trailing-slash ()
  "Should return a path ending with /."
  (let ((result (scratch--repo-default-directory "~/.emacs.d")))
    (should (string-suffix-p "/" result))))

(ert-deftest test-repo-default-directory-expands ()
  "Should expand the path."
  (let ((result (scratch--repo-default-directory "~/.emacs.d")))
    (should-not (string-prefix-p "~" result))))

;; ---------------------------------------------------------------------------
;; scratch--render: checking / fetching state
;; ---------------------------------------------------------------------------

(ert-deftest test-render-checking-state ()
  "Should show CHECKING TODO keyword for repos in checking state."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'checking)
               scratch--repo-statuses)
      (scratch--render)
      (should (string-match-p "\\*\\* CHECKING" (buffer-string))))))

(ert-deftest test-render-fetching-state ()
  "Should show FETCHING TODO keyword for repos in fetching state."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'fetching)
               scratch--repo-statuses)
      (scratch--render)
      (should (string-match-p "\\*\\* FETCHING" (buffer-string))))))

;; ---------------------------------------------------------------------------
;; scratch--render: error state
;; ---------------------------------------------------------------------------

(ert-deftest test-render-error-state ()
  "Should show ERROR TODO keyword and error message."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'error :error "Fetch failed")
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* ERROR" text))
        (should (string-match-p "Fetch failed" text))))))

(ert-deftest test-render-error-state-unknown ()
  "Should show 'unknown' when error message is nil."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'error :error nil)
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* ERROR" text))
        (should (string-match-p "unknown" text))))))

;; ---------------------------------------------------------------------------
;; scratch--render: ready state — clean & up to date
;; ---------------------------------------------------------------------------

(ert-deftest test-render-ready-clean-up-to-date ()
  "Should show UP_TO_DATE keyword with path and branch."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'ready :branch "master" :local "Clean" :behind 0)
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* UP_TO_DATE" text))
        (should (string-match-p "~/.emacs.d on master" text))))))

;; ---------------------------------------------------------------------------
;; scratch--render: ready state — dirty & behind
;; ---------------------------------------------------------------------------

(ert-deftest test-render-ready-behind-remote ()
  "Should show BEHIND keyword with path and branch."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'ready :branch "main" :local "3 modified" :behind 5)
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* BEHIND" text))
        (should (string-match-p "~/.emacs.d on main" text))))))

(ert-deftest test-render-ready-no-pull-when-up-to-date ()
  "Should show UP_TO_DATE and no [Pull] button when behind is 0."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'ready :branch "master" :local "Clean" :behind 0)
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* UP_TO_DATE" text))
        (should-not (string-match-p "\\[Pull\\]" text))))))

(ert-deftest test-render-modified-keyword ()
  "Should show UP_TO_DATE keyword when dirty but not behind (no file detail in simplified render)."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'ready :branch "master" :local "2 modified" :behind 0)
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* UP_TO_DATE" text))
        (should (string-match-p "~/.emacs.d on master" text))))))

;; ---------------------------------------------------------------------------
;; scratch--render: header
;; ---------------------------------------------------------------------------

(ert-deftest test-render-includes-header-quote ()
  "Render should include the lighthouse quote from scratch--header."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'ready :branch "master" :local "Clean" :behind 0)
               scratch--repo-statuses)
      (scratch--render)
      (should (string-match-p "I've always thought they were lighthouses"
                              (buffer-string))))))

(ert-deftest test-render-includes-todo-line ()
  "Render should include the #+TODO keyword line in the header."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "#\\+TODO:" text))
        (should (string-match-p "Repository Status" text))))))

;; ---------------------------------------------------------------------------
;; scratch--render: re-render replaces, doesn't duplicate
;; ---------------------------------------------------------------------------

(ert-deftest test-render-idempotent ()
  "Multiple renders should not duplicate the dashboard section."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'checking)
               scratch--repo-statuses)
      (scratch--render)
      ;; Update and render again
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'ready :branch "master" :local "Clean" :behind 0)
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        ;; Only one "Repository Status" heading
        (should (= 1 (with-temp-buffer
                        (insert text)
                        (goto-char (point-min))
                        (let ((count 0))
                          (while (re-search-forward "^\\* Repository Status" nil t)
                            (setq count (1+ count)))
                          count))))
        ;; Should show final state — no CHECKING heading
        (should-not (string-match-p "^\\*\\* CHECKING" text))
        (should (string-match-p "^\\*\\* UP_TO_DATE" text))))))

;; ---------------------------------------------------------------------------
;; scratch--render: multiple repos
;; ---------------------------------------------------------------------------

(ert-deftest test-render-multiple-repos ()
  "Should render status for each repo in scratch-repos."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d") ("/tmp"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'ready :branch "master" :local "Clean" :behind 0)
               scratch--repo-statuses)
      (puthash (scratch--abbrev-path "/tmp")
               (list :state 'error :error "Not a git repo")
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "UP_TO_DATE.*~/.emacs.d" text))
        (should (string-match-p "ERROR.*/tmp" text))
        (should (string-match-p "Not a git repo" text))))))

;; ---------------------------------------------------------------------------
;; scratch--render: nil / missing status
;; ---------------------------------------------------------------------------

(ert-deftest test-render-nil-status-shows-checking ()
  "Repo with no status entry should show CHECKING keyword."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      ;; Don't put any status — hash table is empty
      (scratch--render)
      (should (string-match-p "\\*\\* CHECKING" (buffer-string))))))

;; ---------------------------------------------------------------------------
;; scratch--render: missing branch shows ?
;; ---------------------------------------------------------------------------

(ert-deftest test-render-missing-branch ()
  "Ready state with nil branch should omit the 'on branch' part."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'ready :branch nil :local "Clean" :behind 0)
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* UP_TO_DATE ~/.emacs.d$" text))
        (should-not (string-match-p " on " text))))))

;; ---------------------------------------------------------------------------
;; org links: pull / commit
;; ---------------------------------------------------------------------------

(ert-deftest test-pull-org-link-registered ()
  "scratch-pull org link type should be registered."
  (should (assoc "scratch-pull" org-link-parameters)))

(ert-deftest test-commit-org-link-registered ()
  "scratch-commit org link type should be registered."
  (should (assoc "scratch-commit" org-link-parameters)))

;; ---------------------------------------------------------------------------
;; scratch-add-repo
;; ---------------------------------------------------------------------------

(ert-deftest test-add-repo-rejects-non-git-dir ()
  "Should error when directory is not a git repo."
  (let ((scratch-repos '(("~/.emacs.d")))
        (dir (make-temp-file "not-a-repo" t)))
    (unwind-protect
        (should-error (scratch-add-repo dir) :type 'user-error)
      (delete-directory dir))))

(ert-deftest test-add-repo-rejects-duplicate ()
  "Should error when repo is already monitored."
  (let ((scratch-repos '(("~/.emacs.d"))))
    (should-error (scratch-add-repo "~/.emacs.d") :type 'user-error)))

(ert-deftest test-add-repo-appends-to-list ()
  "Should append new repo to scratch-repos and persist."
  (with-scratch-buffer ""
    (let* ((scratch-repos '(("~/.emacs.d")))
           (dir (make-temp-file "test-repo" t))
           (saved nil))
      (make-directory (expand-file-name ".git" dir))
      (unwind-protect
          (cl-letf (((symbol-function 'scratch--save-repos)
                     (lambda () (setq saved scratch-repos)))
                    ((symbol-function 'scratch--fetch-repo)
                     (lambda (_repo))))
            (scratch-add-repo dir)
            (should (= 2 (length saved)))
            (should (string= (scratch--abbrev-path dir)
                              (scratch--repo-path (cadr saved)))))
        (delete-directory dir t)))))

;; ---------------------------------------------------------------------------
;; scratch--save-repos / scratch--load-repos
;; ---------------------------------------------------------------------------

(ert-deftest test-save-and-load-repos ()
  "Should round-trip scratch-repos through the config file."
  (let* ((scratch-repos-file (make-temp-file "scratch-repos-test"))
         (scratch-repos '(("~/.emacs.d" . "git@github.com:user/emacs.d.git")
                          ("/tmp/foo"))))
    (unwind-protect
        (progn
          (scratch--save-repos)
          ;; Reset and load back
          (setq scratch-repos nil)
          (scratch--load-repos)
          (should (equal scratch-repos '(("~/.emacs.d" . "git@github.com:user/emacs.d.git")
                                         ("/tmp/foo")))))
      (delete-file scratch-repos-file))))

(ert-deftest test-load-repos-missing-file ()
  "Should leave scratch-repos unchanged when file doesn't exist."
  (let ((scratch-repos-file "/tmp/nonexistent-scratch-repos-file")
        (scratch-repos '(("~/.emacs.d"))))
    (scratch--load-repos)
    (should (equal scratch-repos '(("~/.emacs.d"))))))

;; ---------------------------------------------------------------------------
;; immortal-scratch
;; ---------------------------------------------------------------------------

(ert-deftest test-immortal-scratch-prevents-kill ()
  "immortal-scratch should return nil when current buffer is *scratch*."
  (with-scratch-buffer ""
    (should (null (immortal-scratch)))))

(ert-deftest test-immortal-scratch-allows-other-buffers ()
  "immortal-scratch should return t for non-scratch buffers."
  (with-temp-buffer
    (should (eq t (immortal-scratch)))))

;; ---------------------------------------------------------------------------
;; scratch--render: modified files list
;; ---------------------------------------------------------------------------

(ert-deftest test-render-ready-with-files-shows-heading ()
  "Repos with files should still render as a simple heading."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/.emacs.d"))))
      (puthash (scratch--abbrev-path "~/.emacs.d")
               (list :state 'ready :branch "master"
                     :local "2 modified" :behind 0
                     :files '("src/foo.el" "src/bar.el"))
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* UP_TO_DATE" text))
        (should (string-match-p "~/.emacs.d on master" text))))))


;; ---------------------------------------------------------------------------
;; scratch--repo-path / scratch--repo-remote
;; ---------------------------------------------------------------------------

(ert-deftest test-repo-path-from-cons ()
  "Should return car of cons entry."
  (should (equal "~/foo" (scratch--repo-path '("~/foo" . "git@host:repo")))))

(ert-deftest test-repo-path-from-string ()
  "Should return the string itself for legacy entries."
  (should (equal "~/foo" (scratch--repo-path "~/foo"))))

(ert-deftest test-repo-remote-from-cons ()
  "Should return cdr of cons entry."
  (should (equal "git@host:repo" (scratch--repo-remote '("~/foo" . "git@host:repo")))))

(ert-deftest test-repo-remote-nil-when-missing ()
  "Should return nil when no remote is set."
  (should (null (scratch--repo-remote '("~/foo")))))

;; ---------------------------------------------------------------------------
;; scratch--migrate-repos
;; ---------------------------------------------------------------------------

(ert-deftest test-migrate-repos-converts-flat-list ()
  "Should convert flat string list to alist format."
  (let ((scratch-repos '("~/.emacs.d" "/tmp/foo"))
        (scratch-repos-file (make-temp-file "scratch-repos-migrate")))
    (unwind-protect
        (progn
          (scratch--migrate-repos)
          (should (equal scratch-repos '(("~/.emacs.d") ("/tmp/foo")))))
      (delete-file scratch-repos-file))))

(ert-deftest test-migrate-repos-no-op-for-alist ()
  "Should not modify already-migrated alist."
  (let ((scratch-repos '(("~/.emacs.d" . "git@host:repo"))))
    (scratch--migrate-repos)
    (should (equal scratch-repos '(("~/.emacs.d" . "git@host:repo"))))))

;; ---------------------------------------------------------------------------
;; scratch--render: missing state
;; ---------------------------------------------------------------------------

(ert-deftest test-render-missing-state ()
  "Should show MISSING keyword and clone hint."
  (with-scratch-buffer ""
    (let ((scratch-repos '(("~/nonexistent" . "git@host:repo"))))
      (puthash (scratch--abbrev-path "~/nonexistent")
               (list :state 'missing)
               scratch--repo-statuses)
      (scratch--render)
      (let ((text (buffer-string)))
        (should (string-match-p "\\*\\* MISSING" text))
        (should (string-match-p "clone" text))))))

;; ---------------------------------------------------------------------------
;; keybinding: c for clone
;; ---------------------------------------------------------------------------

(ert-deftest test-clone-keybinding ()
  "Key c should be bound to scratch-clone-repo."
  (should (eq (lookup-key scratch-dashboard-mode-map "c") #'scratch-clone-repo)))

(provide 'test-my-scratch)
;;; test-my-scratch.el ends here
