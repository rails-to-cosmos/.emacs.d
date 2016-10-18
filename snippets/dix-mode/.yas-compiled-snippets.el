;;; Compiled snippets and support files for `dix-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'dix-mode
                     '(("<s" "<section id=\"${1:main}\" type=\"${2:$$(yas-choose-value '(\"standard\" \"inconditional\" \"postblank\" \"preblank\"))}\">\n$0\n</section>\n" "<section> element"
                        (progn
                          (backward-char 2)
                          (and
                           (not
                            (dix-enclosing-is-mono-section))
                           (not
                            (member
                             (dix-enclosing-elt 'noerror)
                             '("l" "r" "i" "g" "sdefs")))))
                        nil nil "/Users/akatovda/.emacs.d/snippets/dix-mode/section" "direct-keybinding" nil)
                       ("<s" "<sdef n=\"$1\"  c=\"$2\"/>\n" "<sdef> element"
                        (progn
                          (backward-char 2)
                          (equal
                           (dix-enclosing-elt 'noerror)
                           "sdefs"))
                        nil nil "/Users/akatovda/.emacs.d/snippets/dix-mode/sdef" "direct-keybinding" nil)
                       ("<s" "<s n=\"$1\"/>$0" "<s> element"
                        (progn
                          (backward-char 2)
                          (member
                           (dix-enclosing-elt 'noerror)
                           '("l" "r" "i" "g")))
                        nil nil "/Users/akatovda/.emacs.d/snippets/dix-mode/s" "direct-keybinding" nil)
                       ("<p" "<pardef n=\"${1:corp/us__n}\">\n  <e>       <p><l>${1:$(dix-yas-pdname-to-suffix yas-text)}</l>      <r>${1:$(dix-yas-pdname-to-suffix yas-text)}${1:$(dix-yas-pdname-to-pos yas-text)}$2</r></p></e>\n  <e>       <p><l>${1:$(dix-yas-pdname-to-suffix yas-text)}$0</l>      <r>${1:$(dix-yas-pdname-to-suffix yas-text)}${1:$(dix-yas-pdname-to-pos yas-text)}$2</r></p></e>\n  <e>       <p><l>${1:$(dix-yas-pdname-to-suffix yas-text)}</l>      <r>${1:$(dix-yas-pdname-to-suffix yas-text)}${1:$(dix-yas-pdname-to-pos yas-text)}$2</r></p></e>\n  <e>       <p><l>${1:$(dix-yas-pdname-to-suffix yas-text)}</l>      <r>${1:$(dix-yas-pdname-to-suffix yas-text)}${1:$(dix-yas-pdname-to-pos yas-text)}$2</r></p></e>\n</pardef>" "<pardef> element"
                        (progn
                          (backward-char 2)
                          (not
                           (equal
                            (dix-enclosing-elt 'noerror)
                            "e")))
                        nil nil "/Users/akatovda/.emacs.d/snippets/dix-mode/pardef" "direct-keybinding" nil)
                       ("<p" "<par n=\"${1::$$(dix-yas-message-pardef (yas-choose-value (dix-pardef-suggest-for (dix-lemma-at-point))))`}\"/>$0" "<par> element"
                        (progn
                          (backward-char 2)
                          (equal
                           (dix-enclosing-elt 'noerror)
                           "e"))
                        nil nil "/Users/akatovda/.emacs.d/snippets/dix-mode/par" "direct-keybinding" nil)
                       ("<p" "<p><l>$1</l> <r>$1$0</r></p>" "<p> element"
                        (progn
                          (backward-char 2)
                          (equal
                           (dix-enclosing-elt 'noerror)
                           "e"))
                        nil nil "/Users/akatovda/.emacs.d/snippets/dix-mode/p" "direct-keybinding" nil)
                       ("<e" "<e> <p><l>$1</l>  <r>$1$0</r></p> </e>\n" "<e> in pardefs"
                        (not
                         (dix-enclosing-is-mono-section))
                        nil nil "/Users/akatovda/.emacs.d/snippets/dix-mode/e-in-pardef" "direct-keybinding" nil)
                       ("<e" "<e lm=\"${1:`(dix-yas-prev-lemma)`}\"> <i>${1:$(dix-yas-lm-to-i)}</i> <par n=\"${3:$$(dix-yas-choose-pdname)}\"/></e>\n$0" "<e> in monodix section"
                        (dix-enclosing-is-mono-section)
                        nil nil "/Users/akatovda/.emacs.d/snippets/dix-mode/e-in-mono-section" "direct-keybinding" nil)))


;;; Do not edit! File generated at Tue Oct 18 15:56:36 2016
