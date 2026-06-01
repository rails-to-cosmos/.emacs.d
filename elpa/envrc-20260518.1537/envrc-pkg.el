;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "envrc" "20260518.1537"
  "Support for `direnv' that operates buffer-locally."
  '((emacs      "28.1")
    (inheritenv "0.1")
    (seq        "2.24"))
  :url "https://github.com/purcell/envrc"
  :commit "1e63a3db367254897a39251944ba68938ec41020"
  :revdesc "1e63a3db3672"
  :keywords '("processes" "tools")
  :authors '(("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Steve Purcell" . "steve@sanityinc.com")))
