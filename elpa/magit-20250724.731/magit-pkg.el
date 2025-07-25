;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250724.731"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1")
    (llama         "1.0.0")
    (magit-section "4.3.8")
    (seq           "2.24")
    (transient     "0.9.3")
    (with-editor   "3.4.4"))
  :url "https://github.com/magit/magit"
  :commit "61c0bd8b405ad96a867706dcb523445817cd92c9"
  :revdesc "61c0bd8b405a"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
