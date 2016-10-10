;;; Compiled snippets and support files for `c-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c-mode
                     '(("uni" "#include <unistd.h>" "unistd" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/unistd" nil nil)
                       ("union" "typedef union {\n        $0\n} ${1:name};" "union" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/union" nil nil)
                       ("str" "#include <string.h>" "string" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/string" nil nil)
                       ("std" "#include <stdlib.h>\n" "stdlib" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/stdlib" nil nil)
                       ("io" "#include <stdio.h>" "stdio" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/stdio" nil nil)
                       ("pr" "printf(\"${1:format string}\"${2: ,a0,a1});" "printf" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/printf" nil nil)
                       ("packed" "__attribute__((__packed__))$0" "packed" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/packed" nil nil)
                       ("malloc" "malloc(sizeof($1)${2: * ${3:3}});\n$0" "malloc" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/malloc" nil nil)
                       ("d" "#define $0" "define" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/define" nil nil)
                       ("compile" "// -*- compile-command: \"${1:gcc -Wall -o ${2:dest} ${3:file}}\" -*-" "compile" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/compile" nil nil)
                       ("ass" "#include <assert.h>\n$0" "assert" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/c-mode/assert" nil nil)))


;;; Do not edit! File generated at Mon Oct 10 03:10:29 2016
