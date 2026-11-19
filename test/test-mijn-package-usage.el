;;; test-mijn-package-usage.el --- Usage persistence tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'mijn-package-usage)

(ert-deftest package-usage-concurrent-sessions-survive-restart ()
  (let* ((dir (make-temp-file "package-usage" t))
         (package-usage-file (expand-file-name "usage.eld" dir)))
    (unwind-protect
        (progn
          (let ((package-usage--session "one") (package-usage--events nil))
            (package-usage--record 'demo 'demo-command 'autoload)
            (package-usage-save)
            (package-usage-save))
          (let ((package-usage--session "two") (package-usage--events nil))
            (package-usage--record 'demo nil 'eager)
            (package-usage-save))
          (let ((saved (package-usage--read)))
            (should (= 2 (length saved)))
            (should (equal '(demo-command)
                           (plist-get (cdr (assoc '(demo . "one") saved)) :commands)))))
      (delete-directory dir t))))

(ert-deftest package-usage-busy-writer-is-not-overwritten ()
  (let* ((dir (make-temp-file "package-usage" t))
         (package-usage-file (expand-file-name "usage.eld" dir))
         (package-usage--events nil))
    (unwind-protect
        (progn
          (make-directory (concat package-usage-file ".lock"))
          (package-usage--record 'demo 'demo-command 'autoload)
          (package-usage-save)
          (should-not (file-exists-p package-usage-file))
          (let ((package-usage--events nil))
            (should (= 1 (length (package-usage--read))))))
      (delete-directory dir t))))

(ert-deftest package-usage-report-includes-every-installed-package ()
  (let ((package-alist `((demo . (,(package-desc-create :name 'demo :version '(1) :archive "gnu")))))
        (package-usage--events nil)
        (package-usage-file "/nonexistent/usage.eld")
        (features (copy-sequence features))
        captured)
    (provide 'table-view)
    (cl-letf (((symbol-function 'table-view-display)
               (lambda (_buffer spec _handlers)
                 (setq captured spec)
                 (get-buffer-create " *usage-test*")))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (progn
            (package-usage-report)
            (let ((rows (alist-get 'rows captured)))
              (should (= 1 (length rows)))
              (should (equal "demo" (alist-get 'package (alist-get 'cells (car rows)))))))
        (kill-buffer " *usage-test*")))))

(ert-deftest package-usage-ownership-matches-directory-semantics ()
  (let* ((root (make-temp-file "package-owners" t))
         (parent (expand-file-name "demo" root))
         (nested (expand-file-name "nested" parent))
         (other (expand-file-name "demo-other" root))
         (alias (expand-file-name "alias" root))
         (package-usage--owner-index nil)
         (package-usage--indexed-packages nil)
         (package-alist
          `((demo ,(package-desc-create :name 'demo :dir parent))
            (nested ,(package-desc-create :name 'nested :dir nested))
            (builtin ,(package-desc-create :name 'builtin)))))
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory other)
          (make-symbolic-link nested alias)
          (dolist (file (list parent nested
                              (expand-file-name "file.el" nested)
                              (expand-file-name "file.el" alias)
                              (expand-file-name "file.el" other)
                              "/usr/share/emacs/builtin.el" nil "relative.el"))
            (should
             (eq (package-usage--owner file)
                 (when (and (stringp file) (file-name-absolute-p file))
                   (cl-loop for (name . descriptors) in package-alist
                            thereis (and (cl-some
                                          (lambda (desc)
                                            (let ((dir (package-desc-dir desc)))
                                              (and dir (file-in-directory-p file dir))))
                                          descriptors)
                                         name))))))
          ;; A replaced package list must not retain the old owner.
          (setq package-alist (cdr package-alist))
          (should (eq 'nested (package-usage--owner (expand-file-name "file.el" alias)))))
      (delete-directory root t))))

(ert-deftest package-usage-ownership-follows-package-upgrades-and-deletions ()
  (let* ((root (make-temp-file "package-versions" t))
         (package-user-dir root)
         (package-alist nil)
         (package-usage--owner-index nil)
         (package-usage--indexed-packages nil)
         (package-selected-packages '(demo)))
    (unwind-protect
        (progn
          (dolist (version '("1" "2"))
            (let ((dir (expand-file-name (concat "demo-" version) root)))
              (make-directory dir)
              (with-temp-file (expand-file-name "demo-pkg.el" dir)
                (insert (format "(define-package \"demo\" \"%s\" \"Test package\")" version)))
              (package-load-descriptor dir)
              (should (eq 'demo (package-usage--owner (expand-file-name "demo.el" dir))))))
          (package-delete (cadr (assq 'demo package-alist)) t t)
          (should-not (package-usage--owner (expand-file-name "demo-2/demo.el" root)))
          (should (eq 'demo (package-usage--owner (expand-file-name "demo-1/demo.el" root)))))
      (delete-directory root t))))
