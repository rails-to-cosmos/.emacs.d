;;; test-mijn-darwin.el --- macOS modifier regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mijn-darwin)

(ert-deftest test-darwin-karabiner-selected-caps-mapping ()
  (let ((file (make-temp-file "karabiner-test-")))
    (unwind-protect
        (dolist (case
                 '(("{\"profiles\":[{\"selected\":true,\"simple_modifications\":[{\"from\":{\"key_code\":\"caps_lock\"},\"to\":[{\"key_code\":\"left_command\"}]}]}]}" . t)
                   ("{\"profiles\":[{\"selected\":false,\"simple_modifications\":[{\"from\":{\"key_code\":\"caps_lock\"},\"to\":[{\"key_code\":\"left_command\"}]}]},{\"selected\":true}]}" . nil)
                   ("{\"profiles\":[{\"selected\":true,\"simple_modifications\":[{\"from\":{\"key_code\":\"caps_lock\"},\"to\":[{\"key_code\":\"left_control\"}]}]}]}" . nil)
                   ("{}" . nil)
                   ("invalid json" . nil)))
          (with-temp-file file (insert (car case)))
          (should (eq (mijn-darwin-caps-is-command-p file) (cdr case))))
      (delete-file file))
    (should-not (mijn-darwin-caps-is-command-p file))))

(ert-deftest test-darwin-modifiers-with-and-without-karabiner ()
  (dolist (remapped '(nil t))
    (let ((system-type 'darwin)
          (mac-command-modifier nil)
          (mac-right-command-modifier nil)
          (mac-option-modifier nil))
      (cl-letf (((symbol-function 'json-read-file)
                 (lambda (&rest _)
                   (when remapped
                     '((profiles . (((selected . t)
                                     (simple_modifications .
                                      (((from . ((key_code . "caps_lock")))
                                        (to . (((key_code . "left_command")))))))))))))))
        (load "mijn-darwin" nil t))
      (should (eq mac-command-modifier (if remapped 'control 'meta)))
      (should (eq mac-right-command-modifier (if remapped 'meta 'left)))
      (should (eq mac-option-modifier 'meta)))))
