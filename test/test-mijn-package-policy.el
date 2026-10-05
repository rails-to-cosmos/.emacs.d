;;; test-mijn-package-policy.el --- Package policy tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'mijn-package-policy)

(ert-deftest test-package-policy-configures-all-archives ()
  (let (package-archives)
    (mijn-configure-package-archives)
    (should (equal package-archives mijn-package-archives))))

(ert-deftest test-package-update-count-refreshes-and-reports-only ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'package-initialize)
               (lambda () (push 'initialize calls)))
              ((symbol-function 'package-refresh-contents)
               (lambda () (push 'refresh calls)))
              ((symbol-function 'package--upgradeable-packages)
               (lambda (&optional _include-builtins)
                 (push 'count calls)
                 '(alpha beta gamma)))
              ((symbol-function 'package-upgrade-all)
               (lambda () (ert-fail "reporting must not upgrade packages"))))
      (should (= 3 (mijn-package-refresh-upgrade-count)))
      (should (equal '(initialize refresh count) (nreverse calls))))))
