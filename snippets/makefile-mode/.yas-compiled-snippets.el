;;; Compiled snippets and support files for `makefile-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'makefile-mode
                     '(("cl" "clean:\n	${1:rm -r ${2:\\$(${3:OUTDIR})}}\n$0\n" "clean" nil nil
                        ((yas-indent-line 'fixed))
                        "/Volumes/Main/Users/akatovda/.emacs.d/snippets/makefile-mode/clean" nil nil)
                       ("all" "all:\n        $0" "all" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/makefile-mode/all" nil nil)))


;;; Do not edit! File generated at Mon Oct 10 03:10:30 2016
