;;; Compiled snippets and support files for `typerex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'typerex-mode
                     '(("wh" "while ${1:cond} do\n      $0\ndone" "while" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/while" nil nil)
                       ("val" "val ${1:fun} : $0" "val" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/val" nil nil)
                       ("type" "type ${1:expr} =\n     | $0" "type_type" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/type_type" nil nil)
                       ("type" "type ${1:name} = {${2:var}: ${3:int}$0}" "type_record" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/type" nil nil)
                       ("try" "try\n        $0\nwith\n        $1" "try" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/try" nil nil)
                       ("rec" "let rec ${1:fun} ${2:args} =\n    $0" "rec" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/rec" nil nil)
                       ("pr" "Printf.printf \"${1:string}\" ${2:vals};;" "printf" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/printf" nil nil)
                       ("op" "open ${1:Module}" "open" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/open" nil nil)
                       ("mod" "module ${1:A} =\n       struct\n       ${2:type t = { name : string; phone : string }}\n       $0\nend;;\n" "module" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/module" nil nil)
                       ("match" "match ${1:to_match} with\n      | ${2:matching} -> $0" "match" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/match" nil nil)
                       ("m" "let main =\n    $0" "main" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/main" nil nil)
                       ("l" "[? $1 | $0 ?]" "list_comprehension" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/list_comprehension" nil nil)
                       ("let" "let ${1:var} $0" "let" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/let" nil nil)
                       ("if" "if ${1:cond} then\n   $2\nelse\n        $0" "ifthenelse" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/ifthenelse" nil nil)
                       ("if" "if ${1:cond} then\n   $0\n" "ifthen" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/ifthen" nil nil)
                       ("|" "| ${1:match} -> $0" "guard" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/guard" nil nil)
                       ("fun" "fun ${1:args} -> $0" "fun" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/fun" nil nil)
                       ("for" "for ${1:cond} do\n    $0\ndone" "for" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/for" nil nil)
                       ("d" "(* $0 *)" "docstring" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/docstring" nil nil)
                       ("as" "assert $0;;" "assert" nil nil nil "/Volumes/Main/Users/akatovda/.emacs.d/snippets/typerex-mode/assert" nil nil)))


;;; Do not edit! File generated at Mon Oct 10 03:10:30 2016
