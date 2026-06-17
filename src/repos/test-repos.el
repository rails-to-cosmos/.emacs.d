;;; test-repos.el --- Tests for repos.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'repos)

;; ---------------------------------------------------------------------------
;; Helper
;; ---------------------------------------------------------------------------

(defmacro with-clean-repos (&rest body)
  "Run BODY with a fresh `repos--statuses' and a restorable `repos-list'."
  (declare (indent 0))
  `(let ((repos-list repos-list)
         (repos--statuses (make-hash-table :test 'equal)))
     ,@body))

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

;; ---------------------------------------------------------------------------
;; Status keyword
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-todo-kw-checking ()
  (with-clean-repos
    (puthash "~/r" '((state . "checking")) repos--statuses)
    (should (equal "CHECKING" (repos--todo-kw "~/r")))))

(ert-deftest test-repos-todo-kw-missing ()
  (with-clean-repos
    (puthash "~/r" '((state . "missing")) repos--statuses)
    (should (equal "MISSING" (repos--todo-kw "~/r")))))

(ert-deftest test-repos-todo-kw-error ()
  (with-clean-repos
    (puthash "~/r" '((state . "error") (error . "boom")) repos--statuses)
    (should (equal "ERROR" (repos--todo-kw "~/r")))))

(ert-deftest test-repos-todo-kw-behind ()
  (with-clean-repos
    (puthash "~/r" '((state . "ready") (behind . 2) (modified . 0) (untracked . 0)) repos--statuses)
    (should (equal "BEHIND" (repos--todo-kw "~/r")))))

(ert-deftest test-repos-todo-kw-modified ()
  (with-clean-repos
    (puthash "~/r" '((state . "ready") (behind . 0) (modified . 1) (untracked . 0)) repos--statuses)
    (should (equal "MODIFIED" (repos--todo-kw "~/r")))))

(ert-deftest test-repos-todo-kw-untracked ()
  (with-clean-repos
    (puthash "~/r" '((state . "ready") (behind . 0) (modified . 0) (untracked . 3)) repos--statuses)
    (should (equal "UNTRACKED" (repos--todo-kw "~/r")))))

(ert-deftest test-repos-todo-kw-up-to-date ()
  (with-clean-repos
    (puthash "~/r" '((state . "ready") (behind . 0) (modified . 0) (untracked . 0)) repos--statuses)
    (should (equal "UP_TO_DATE" (repos--todo-kw "~/r")))))

;; ---------------------------------------------------------------------------
;; Table-view consumer row mapping
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-table-row-behind ()
  "Row cells should reflect a behind repo."
  (with-clean-repos
    (puthash "~/r" '((state . "ready") (branch . "main") (behind . 3)
                     (modified . 0) (untracked . 0)) repos--statuses)
    (let* ((row (repos-table--row "~/r"))
           (cells (alist-get 'cells row)))
      (should (equal "~/r" (alist-get 'id row)))
      (should (equal "BEHIND" (alist-get 'state cells)))
      (should (equal "~/r" (alist-get 'name cells)))
      (should (equal "main" (alist-get 'branch cells)))
      (should (equal 3 (alist-get 'behind cells))))))

(ert-deftest test-repos-table-row-error ()
  "Error rows surface the error message in the local cell."
  (with-clean-repos
    (puthash "~/r" '((state . "error") (error . "boom")) repos--statuses)
    (let ((cells (alist-get 'cells (repos-table--row "~/r"))))
      (should (equal "ERROR" (alist-get 'state cells)))
      (should (equal "boom" (alist-get 'local cells))))))

(ert-deftest test-repos-table-row-missing ()
  "Missing rows surface a clone hint."
  (with-clean-repos
    (puthash "~/r" '((state . "missing")) repos--statuses)
    (let ((cells (alist-get 'cells (repos-table--row "~/r"))))
      (should (equal "MISSING" (alist-get 'state cells)))
      (should (equal "clone with c" (alist-get 'local cells))))))

;; ---------------------------------------------------------------------------
;; Keybinding
;; ---------------------------------------------------------------------------

(ert-deftest test-repos-dashboard-binding ()
  "C-x y p opens the table dashboard."
  (should (eq (lookup-key global-map (kbd "C-x y p")) #'repos-table)))

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
