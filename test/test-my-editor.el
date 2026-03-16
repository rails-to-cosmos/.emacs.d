;;; test-my-editor.el --- Tests for init-editor functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)

;; ---------------------------------------------------------------------------
;; Helper: extract a defun from a source file without loading the whole file
;; ---------------------------------------------------------------------------

(defun test--eval-defun-from-file (file fn-name)
  "Read FILE and evaluate the defun/cl-defun for FN-NAME."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((pattern (format "^(\\(?:cl-\\)?defun %s[ \t\n(]"
                           (regexp-quote (symbol-name fn-name)))))
      (when (re-search-forward pattern nil t)
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t)))))

(defvar test--editor-file
  (expand-file-name "../src/my-editor.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

;; Load just the functions we want to test
(test--eval-defun-from-file test--editor-file 'increment-number-at-point)
(test--eval-defun-from-file test--editor-file 'decrement-number-at-point)
(test--eval-defun-from-file test--editor-file 'my/join-region-into-one-line)
(test--eval-defun-from-file test--editor-file 'my-remove-duplicate-lines-in-region)

;; ---------------------------------------------------------------------------
;; increment-number-at-point
;; ---------------------------------------------------------------------------

(ert-deftest test-increment-number-at-point ()
  "Should increment the number under point."
  (with-temp-buffer
    (insert "value: 42 end")
    (goto-char 10) ; on "42"
    (increment-number-at-point)
    (should (string= "value: 43 end" (buffer-string)))))

(ert-deftest test-increment-negative-number ()
  "Should increment a negative number."
  (with-temp-buffer
    (insert "-5")
    (goto-char 2)
    (increment-number-at-point)
    (should (string= "-4" (buffer-string)))))

(ert-deftest test-increment-zero ()
  "Should increment 0 to 1."
  (with-temp-buffer
    (insert "0")
    (goto-char 1)
    (increment-number-at-point)
    (should (string= "1" (buffer-string)))))

(ert-deftest test-increment-no-number-signals-error ()
  "Should signal error when no number at point."
  (with-temp-buffer
    (insert "abc")
    (goto-char 2)
    (should-error (increment-number-at-point))))

;; ---------------------------------------------------------------------------
;; decrement-number-at-point
;; ---------------------------------------------------------------------------

(ert-deftest test-decrement-number-at-point ()
  "Should decrement the number under point."
  (with-temp-buffer
    (insert "value: 42 end")
    (goto-char 10)
    (decrement-number-at-point)
    (should (string= "value: 41 end" (buffer-string)))))

(ert-deftest test-decrement-zero ()
  "Should decrement 0 to -1."
  (with-temp-buffer
    (insert "0")
    (goto-char 1)
    (decrement-number-at-point)
    (should (string= "-1" (buffer-string)))))

(ert-deftest test-decrement-no-number-signals-error ()
  "Should signal error when no number at point."
  (with-temp-buffer
    (insert "abc")
    (goto-char 2)
    (should-error (decrement-number-at-point))))

;; ---------------------------------------------------------------------------
;; my/join-region-into-one-line
;; ---------------------------------------------------------------------------

(ert-deftest test-join-region-into-one-line ()
  "Should join multiple lines into one, adding to kill ring."
  (with-temp-buffer
    (insert "line one\n  line two\n    line three")
    (let ((kill-ring nil))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (my/join-region-into-one-line (point-min) (point-max))
      (should (string= "line one line two line three" (car kill-ring))))))

(ert-deftest test-join-region-single-line ()
  "Should handle a single line without change."
  (with-temp-buffer
    (insert "single line")
    (let ((kill-ring nil))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (my/join-region-into-one-line (point-min) (point-max))
      (should (string= "single line" (car kill-ring))))))

;; ---------------------------------------------------------------------------
;; my-remove-duplicate-lines-in-region
;; ---------------------------------------------------------------------------

(ert-deftest test-remove-duplicate-lines ()
  "Should remove duplicate lines keeping first occurrence."
  (with-temp-buffer
    (insert "aaa\nbbb\naaa\nccc\nbbb")
    (my-remove-duplicate-lines-in-region (point-min) (point-max))
    (should (string= "aaa\nbbb\nccc" (buffer-string)))))

(ert-deftest test-remove-duplicate-lines-no-duplicates ()
  "Should leave unique lines unchanged."
  (with-temp-buffer
    (insert "one\ntwo\nthree")
    (my-remove-duplicate-lines-in-region (point-min) (point-max))
    (should (string= "one\ntwo\nthree" (buffer-string)))))

(ert-deftest test-remove-duplicate-lines-all-same ()
  "Should collapse all identical lines to one."
  (with-temp-buffer
    (insert "x\nx\nx")
    (my-remove-duplicate-lines-in-region (point-min) (point-max))
    (should (string= "x" (buffer-string)))))

(provide 'test-my-editor)
;;; test-my-editor.el ends here
