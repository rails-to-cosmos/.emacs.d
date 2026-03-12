;;; test-init-dired.el --- Tests for init-dired functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'dash)

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

(defvar test--dired-file
  (expand-file-name "../src/init-dired.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

;; Load just the pure functions
(test--eval-defun-from-file test--dired-file 'my-clean-file-name)
(test--eval-defun-from-file test--dired-file 'my-transliterate-ru-to-en)

;; ---------------------------------------------------------------------------
;; my-clean-file-name
;; ---------------------------------------------------------------------------

(ert-deftest test-clean-file-name-basic ()
  "Should lowercase and normalize separators."
  (should (string= "hello-world" (my-clean-file-name "Hello World"))))

(ert-deftest test-clean-file-name-removes-hash ()
  "Should remove trailing [hash] from name."
  (should (string= "document" (my-clean-file-name "Document [abc123]"))))

(ert-deftest test-clean-file-name-collapses-dashes ()
  "Should collapse multiple dashes into one."
  (should (string= "a-b" (my-clean-file-name "a---b"))))

(ert-deftest test-clean-file-name-strips-leading-trailing-dashes ()
  "Should remove leading and trailing dashes."
  (should (string= "hello" (my-clean-file-name "-hello-"))))

(ert-deftest test-clean-file-name-special-characters ()
  "Should replace special characters with dashes."
  (should (string= "file-name-2024" (my-clean-file-name "file@name#2024!"))))

(ert-deftest test-clean-file-name-preserves-cyrillic ()
  "Should preserve Cyrillic characters."
  (should (string= "привет-мир" (my-clean-file-name "Привет Мир"))))

(ert-deftest test-clean-file-name-already-clean ()
  "Should not change a name that is already clean."
  (should (string= "clean-name" (my-clean-file-name "clean-name"))))

;; ---------------------------------------------------------------------------
;; my-transliterate-ru-to-en
;; ---------------------------------------------------------------------------

(ert-deftest test-transliterate-basic ()
  "Should transliterate basic Russian text."
  (should (string= "privet" (my-transliterate-ru-to-en "привет"))))

(ert-deftest test-transliterate-uppercase ()
  "Should transliterate uppercase Russian characters.
Note: lowercase entries match first due to case-fold-search,
so uppercase map entries only apply to chars without lowercase equivalents."
  (let ((case-fold-search nil))
    (should (string= "Moskva" (my-transliterate-ru-to-en "Москва")))))

(ert-deftest test-transliterate-mixed ()
  "Should leave non-Russian characters untouched."
  (should (string= "hello mir" (my-transliterate-ru-to-en "hello мир"))))

(ert-deftest test-transliterate-special-chars ()
  "Should handle ъ and ь (hard/soft signs) as empty strings."
  (should (string= "obezyana" (my-transliterate-ru-to-en "обезьяна"))))

(ert-deftest test-transliterate-complex-consonants ()
  "Should transliterate ж, ш, щ, ч, ц correctly."
  (should (string= "zhenshchina" (my-transliterate-ru-to-en "женщина"))))

(ert-deftest test-transliterate-empty-string ()
  "Should handle empty string."
  (should (string= "" (my-transliterate-ru-to-en ""))))

(provide 'test-init-dired)
;;; test-init-dired.el ends here
