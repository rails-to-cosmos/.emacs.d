(require 'ert)
(require 'mijn-packages)

(ert-deftest test-package-registration-uses-cached-archives ()
  (let ((package-archive-contents '((available . nil)))
        (mijn-required-packages nil)
        (package-selected-packages nil)
        (mijn-package-archives-refreshed nil)
        refreshes)
    (cl-letf (((symbol-function 'package-installed-p) (lambda (&rest _) nil))
              ((symbol-function 'package-refresh-contents)
               (lambda () (push t refreshes)))
              ((symbol-function 'package-install)
               (lambda (&rest _) (error "Obsolete selection is unavailable"))))
      (mijn-register-packages '(obsolete-selection)))
    (should-not refreshes)
    (should (memq 'obsolete-selection package-selected-packages))))

(ert-deftest test-up-registers-name-and-every-ensure-target ()
  (let ((expanded
         (macroexpand-1
          '(up python
             :ensure nil
             :defer
             :init configure-python
             :ensure mise
             :ensure yasnippet-capf))))
    (should
     (equal (cadr expanded)
            '(mijn-register-packages
              '(python mise yasnippet-capf) nil)))))

(ert-deftest test-up-registers-vc-package-separately ()
  (let ((expanded
         (macroexpand-1
          '(up darr
             :vc (:url "https://example.test/darr.git")))))
    (should
     (equal (cadr expanded)
            '(mijn-register-packages '(darr) 'darr)))))
