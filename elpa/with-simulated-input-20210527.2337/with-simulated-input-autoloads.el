;;; with-simulated-input-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from with-simulated-input.el

(autoload 'with-simulated-input-1 "with-simulated-input" "\
Internal `with-simulated-input' helper.

MAIN is a zero-argument function containing the body forms to be
evaluated, and KEYS is a list of key sequences (as strings) or
other actions to simulate user interaction (as zero-argument
functions, which are called only for their side effects).

(fn MAIN &rest KEYS)")
(autoload 'with-simulated-input "with-simulated-input" "\
Eval BODY forms with KEYS as simulated input.

This macro is intended for automated testing of normally
interactive functions by simulating input. If BODY tries to read
user input (e.g. via `completing-read'), it will read input
events from KEYS instead, as if the user had manually typed those
keys after initiating evaluation of BODY.

KEYS should be a string representing a sequence of key presses,
in the format understood by `kbd'. In the most common case of
typing in some text and pressing RET, KEYS would be something
like `\"hello RET\"'. Note that spaces must be indicated
explicitly using `SPC', e.g. `\"hello SPC world RET\"'.

KEYS can also be a single character, which is equivalent to a
string of length 1.

KEYS can also be a list of strings (or characters), which will be
used as consecutive inputs. (This list should not be quoted,
since `with-simulated-input' is a macro.) Elements of the list
can also be function calls, which will be evaluated at that point
in the input sequence. This can be used as an alternative to
writing out a full key sequence. For example, `\"hello SPC world
RET\"' could also be written as:

    `((insert \"hello world\") \"RET\")'

It can also be used to implement more complex logic, such as
conditionally inserting a string. Note that the return value of
any function call in KEYS is ignored, so the function should
actually perform some kind of action, or else it will have no
effect.

Lastly, KEYS can also be the name of a variable whose value is a
string. The variable's value will be used as described above.

If BODY tries to read more input events than KEYS provides, an
error is signaled. This is to ensure that BODY will never get
stuck waiting for input, since this macro is intended for
non-interactive use. If BODY does not consume all the input
events in KEYS, the remaining input events in KEYS are discarded,
and any remaining function calls in KEYS are never evaluated. In
particular, if KEYS is nil, then an error will be signaled if
BODY attempts to read any input, and if BODY is nil, a constant
expression, or an expression that does not read any input, then
KEYS will be ignored completely.

Any errors generated by any means during the evaluation of BODY
or the evaluation of function calls in KEYS are propagated
normally.

The return value is the last form in BODY, as if it was wrapped
in `progn'.

(Note: KEYS supports some additional semantics for
backward-compatibilty reasons. These semantics are considered
deprecated and are left intentionally undocumented. They should
not be used in newly written code, since they will stop working
in a future release.)

(fn KEYS &rest BODY)" nil t)
(function-put 'with-simulated-input 'lisp-indent-function 1)
(register-definition-prefixes "with-simulated-input" '("current-idle-time@simulate-idle-time" "with-simulated-input-unload-function" "wsi-"))

;;; End of scraped data

(provide 'with-simulated-input-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; with-simulated-input-autoloads.el ends here
