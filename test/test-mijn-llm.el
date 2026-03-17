;;; test-my-llm.el --- Tests for my-llm -*- lexical-binding: t; -*-

(require 'ert)
(require 'mijn-llm)

;; ---------------------------------------------------------------------------
;; llm--project-root
;; ---------------------------------------------------------------------------

(ert-deftest test-project-root-falls-back-to-dir ()
  "When no marker files exist, return the directory itself."
  (let ((dir (make-temp-file "llm-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (should (equal (llm--project-root) (file-name-as-directory dir))))
      (delete-directory dir t))))

(ert-deftest test-project-root-finds-git ()
  "Should find root containing .git directory."
  (let ((dir (make-temp-file "llm-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (let ((sub (expand-file-name "src/" dir)))
            (make-directory sub t)
            (let ((default-directory sub))
              (should (equal (llm--project-root) (file-name-as-directory dir))))))
      (delete-directory dir t))))

;; ---------------------------------------------------------------------------
;; llm--write-context-file
;; ---------------------------------------------------------------------------

(ert-deftest test-write-context-file ()
  "Should write text to a temp file and return its path."
  (let ((file (llm--write-context-file "hello world")))
    (unwind-protect
        (progn
          (should (file-exists-p file))
          (should (equal "hello world"
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (ignore-errors (delete-file file)))))

;; ---------------------------------------------------------------------------
;; llm--diff-added-lines
;; ---------------------------------------------------------------------------

(ert-deftest test-diff-added-lines-no-change ()
  "No changes should return empty list."
  (should (null (llm--diff-added-lines "foo\nbar\n" "foo\nbar\n"))))

(ert-deftest test-diff-added-lines-detects-additions ()
  "Should detect added lines."
  (let ((lines (llm--diff-added-lines "a\nb\n" "a\nx\nb\n")))
    (should (equal '(2) lines))))

(provide 'test-my-llm)
;;; test-my-llm.el ends here
