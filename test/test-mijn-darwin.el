;;; test-mijn-darwin.el --- macOS modifier regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mijn-darwin)

(ert-deftest test-darwin-modifiers-are-independent-of-karabiner-and-prior-state ()
  (dolist (old '(control meta super nil))
    (let ((system-type 'darwin)
          (mac-control-modifier old)
          (mac-command-modifier old)
          (mac-right-command-modifier old)
          (mac-option-modifier old)
          (ns-control-modifier old)
          (ns-command-modifier old)
          (ns-right-command-modifier old)
          (ns-alternate-modifier old))
      (cl-letf (((symbol-function 'json-read-file)
                 (lambda (&rest _) (ert-fail "keyboard policy read historical Karabiner settings"))))
        (load "mijn-darwin" nil t))
      (should (mijn-darwin-modifiers-ready-p))
      (should (equal (mapcar #'symbol-value
                            '(mac-control-modifier ns-control-modifier
                              mac-command-modifier ns-command-modifier
                              mac-right-command-modifier ns-right-command-modifier
                              mac-option-modifier ns-alternate-modifier))
                     '(control control meta meta left left meta meta)))
      (setq ns-control-modifier 'meta)
      (should-not (mijn-darwin-modifiers-ready-p)))))

(ert-deftest test-darwin-modifiers-leave-other-platforms-unchanged ()
  (let ((system-type 'gnu/linux)
        (mac-command-modifier 'super)
        (mac-control-modifier 'control))
    (load "mijn-darwin" nil t)
    (should (eq mac-command-modifier 'super))
    (should (eq mac-control-modifier 'control))
    (should-not (mijn-darwin-modifiers-ready-p))))
