(require 'ert)
(require 'mijn-packages)

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
