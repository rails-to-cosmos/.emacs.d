;;; test-my-dired.el --- Tests for init-dired functions -*- lexical-binding: t; -*-

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
  (expand-file-name "../src/mijn-dired.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

;; Load just the pure functions
(test--eval-defun-from-file test--dired-file 'my-transliterate-ru-to-en)
(test--eval-defun-from-file test--dired-file 'my--normalize-name)
(test--eval-defun-from-file test--dired-file 'my-normalized-path)
(test--eval-defun-from-file test--dired-file 'my--directory-normalization-plan)
(test--eval-defun-from-file test--dired-file 'my--numbered-normalized-target)
(test--eval-defun-from-file test--dired-file 'my--disambiguate-directory-normalization-plan)
(test--eval-defun-from-file test--dired-file 'my--rename-path-case-safely)
(test--eval-defun-from-file test--dired-file 'my-normalize-directory-names)
(test--eval-defun-from-file test--dired-file 'my-dired-standardize)

;; ---------------------------------------------------------------------------
;; my-normalize-directory-names
;; ---------------------------------------------------------------------------

(ert-deftest test-normalized-path-transliterates-and-preserves-extension ()
  "The normalized path should be Latin, lowercase, and keep its extension."
  (let ((directory (make-temp-file "normalize-name-test-" t)))
    (unwind-protect
        (progn
          (should
           (equal (my-normalized-path
                   (expand-file-name "Привет Мир.TXT" directory))
                  (expand-file-name "privet-mir.txt" directory)))
          (should
           (equal (my-normalized-path
                   (expand-file-name ".Привет Мир.TXT" directory))
                  (expand-file-name ".privet-mir.txt" directory)))
          (should
           (equal (my-normalized-path
                   (expand-file-name "Report [FINAL].PDF" directory))
                  (expand-file-name "report-final.pdf" directory))))
      (delete-directory directory t))))

(ert-deftest test-normalize-directory-names-is-nonrecursive-by-default ()
  "Without a prefix, normalize immediate entries and leave descendants alone."
  (let ((directory (make-temp-file "normalize-name-test-" t)))
    (unwind-protect
        (let* ((default-directory directory)
               (subdirectory (expand-file-name "Some Folder" directory))
               (nested (expand-file-name "Nested File.TXT" subdirectory)))
          (make-directory subdirectory)
          (with-temp-file nested)
          (with-temp-file (expand-file-name "Top File.TXT" directory))
          (my-normalize-directory-names)
          (should (file-exists-p (expand-file-name "top-file.txt" directory)))
          (should (file-exists-p
                   (expand-file-name "some-folder/Nested File.TXT" directory))))
      (delete-directory directory t))))

(ert-deftest test-normalize-directory-names-recursively-transliterates ()
  "With a prefix, normalize directory names and all descendants."
  (let ((directory (make-temp-file "normalize-name-test-" t)))
    (unwind-protect
        (let* ((default-directory directory)
               (subdirectory (expand-file-name "Папка Тест" directory)))
          (make-directory subdirectory)
          (with-temp-file (expand-file-name "Файл Один.TXT" subdirectory))
          (my-normalize-directory-names t)
          (should (file-exists-p
                   (expand-file-name "papka-test/fail-odin.txt" directory))))
      (delete-directory directory t))))

(ert-deftest test-normalize-directory-names-numbers-collisions ()
  "Add a numeric suffix when two entries share a normalized name."
  (let ((directory (make-temp-file "normalize-name-test-" t)))
    (unwind-protect
        (let ((default-directory directory))
          (with-temp-file (expand-file-name "A B" directory))
          (with-temp-file (expand-file-name "a-b" directory))
          (my-normalize-directory-names)
          (should (file-exists-p (expand-file-name "a-b" directory)))
          (should (file-exists-p (expand-file-name "a-b-2" directory))))
      (delete-directory directory t))))

(ert-deftest test-dired-standardize-uses-canonical-recursive-normalizer ()
  "The compatibility command should delegate naming to the canonical pipeline."
  (let (recursive)
    (cl-letf (((symbol-function 'my-dired-normalize-permissions) #'ignore)
              ((symbol-function 'my-dired-cleanup) #'ignore)
              ((symbol-function 'my-normalize-directory-names)
               (lambda (argument) (setq recursive argument))))
      (my-dired-standardize))
    (should recursive)))

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

(provide 'test-my-dired)
;;; test-my-dired.el ends here
