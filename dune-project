(lang dune 3.15)

(using menhir 3.0)

(name mlisp)

(source
 (github mizurennanako/mlisp))

(authors "Cherry Sigma")

(license public domain)

(documentation https://url/to/documentation)

(package
 (name mlisp)
 (synopsis "mlisp")
 (description "M-Expr based LISP")
 (depends ocaml dune menhirLib ppx_sexp_conv sexplib))
