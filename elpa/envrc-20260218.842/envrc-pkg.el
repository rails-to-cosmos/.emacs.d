;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "envrc" "20260218.842"
  "Support for `direnv' that operates buffer-locally."
  '((emacs      "27.1")
    (inheritenv "0.1")
    (seq        "2.24"))
  :url "https://github.com/purcell/envrc"
  :commit "f44353c42c0794cdc6629c83a923d1689f33469f"
  :revdesc "f44353c42c07"
  :keywords '("processes" "tools")
  :authors '(("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Steve Purcell" . "steve@sanityinc.com")))
