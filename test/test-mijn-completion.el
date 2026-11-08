;;; test-mijn-completion.el --- Completion macro tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'mijn-completion)

(ert-deftest test-completion-without-docstring-is-interactive ()
  (unwind-protect
      (progn
        (eval '(my-define-completion test-completion-arrow "->" (insert "→") t))
        (should (commandp 'test-completion-arrow))
        (with-temp-buffer
          (insert "->")
          (call-interactively 'test-completion-arrow)
          (should (equal (buffer-string) "→"))))
    (remove-hook 'post-self-insert-hook 'test-completion-arrow)
    (fmakunbound 'test-completion-arrow)))
