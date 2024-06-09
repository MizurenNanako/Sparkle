(lang dune 3.15)

(using menhir 3.0)

(name mlisp)

(source
 (github mizurennanako/spml))

(authors "Cherry Sigma")

(license public domain)

(documentation https://url/to/documentation)

(package
 (name mlisp)
 (synopsis "mlisp")
 (description "Sparkle is an M-Expr based LISP")
 (depends ocaml dune menhirLib ppx_sexp_conv sexplib))
