;;; go-guru-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from go-guru.el

(autoload 'go-guru-set-scope "go-guru" "\
Set the scope for the Go guru, prompting the user to edit the previous scope.

The scope restricts analysis to the specified packages.
Its value is a comma-separated list of patterns of these forms:
	golang.org/x/tools/cmd/guru     # a single package
	golang.org/x/tools/...          # all packages beneath dir
	...                             # the entire workspace.

A pattern preceded by '-' is negative, so the scope
	encoding/...,-encoding/xml
matches all encoding packages except encoding/xml." t)
(autoload 'go-guru-callees "go-guru" "\
Show possible callees of the function call at the current point." t)
(autoload 'go-guru-callers "go-guru" "\
Show the set of callers of the function containing the current point." t)
(autoload 'go-guru-callstack "go-guru" "\
Show an arbitrary path from a root of the call graph to the
function containing the current point." t)
(autoload 'go-guru-definition "go-guru" "\
Jump to the definition of the selected identifier.

(fn &optional OTHER-WINDOW)" t)
(autoload 'go-guru-definition-other-window "go-guru" "\
Jump to the defintion of the selected identifier in another window" t)
(autoload 'go-guru-describe "go-guru" "\
Describe the selected syntax, its kind, type and methods." t)
(autoload 'go-guru-pointsto "go-guru" "\
Show what the selected expression points to." t)
(autoload 'go-guru-implements "go-guru" "\
Describe the 'implements' relation for types in the package
containing the current point." t)
(autoload 'go-guru-freevars "go-guru" "\
Enumerate the free variables of the current selection." t)
(autoload 'go-guru-peers "go-guru" "\
Enumerate the set of possible corresponding sends/receives for
this channel receive/send operation." t)
(autoload 'go-guru-referrers "go-guru" "\
Enumerate all references to the object denoted by the selected
identifier." t)
(autoload 'go-guru-whicherrs "go-guru" "\
Show globals, constants and types to which the selected
expression (of type 'error') may refer." t)
(autoload 'go-guru-unhighlight-identifiers "go-guru" "\
Remove highlights from previously highlighted identifier.")
(autoload 'go-guru-hl-identifier "go-guru" "\
Highlight all instances of the identifier under point. Removes
highlights from previously highlighted identifier." t)
(autoload 'go-guru-hl-identifier-mode "go-guru" "\
Highlight instances of the identifier at point after a short

timeout.

This is a minor mode.  If called interactively, toggle the
`Go-Guru-Hl-Identifier mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `go-guru-hl-identifier-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "go-guru" '("go-guru-"))

;;; End of scraped data

(provide 'go-guru-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; go-guru-autoloads.el ends here
