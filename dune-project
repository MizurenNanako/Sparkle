(lang dune 3.15)

(using menhir 3.0)

(name sparkle-lang)

(source
 (github mizurennanako/spml))

(authors "Cherry Sigma")

(license public domain)

(documentation https://url/to/documentation)

; (package
;  (name mmips)
;  (synopsis "mmips")
;  (description "generate mips instruction from c-ish input")
;  (depends ocaml dune))

(package
 (name sparkle-lang)
 (synopsis "spml")
 (description
  "Sparkle is an experimental programming language focus on stack machine")
 (depends ocaml dune menhirLib ppx_sexp_conv sexplib mmips))
