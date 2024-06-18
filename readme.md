# m-lisp translater

### Syntax:

#### Let-binding

As in ml `let xxx = yyy in zzz`, let bindings in this syntax should be `xxx := yyy => zzz`,
with type annotation in ml `let xxx: ttt = yyy in zzz`, in this syntax it should be `xxx: ttt = yyy => zzz`

#### Annotation

As in toplevel ml `let val xxx: ttt in zzz`, type annotations in this syntax should be `xxx: ttt => zzz`, also in top level.
This also declared the name, for recursive definitions like functions, this is necessary.

#### Branching

As in Commom Lisp `(cond (p1 e1) (p2 e2) ... ('t en))`, in this syntax it should be `p1 -> e1 | p2 -> e2 | ... | _ -> en`, where wildcard must be the last predicate.

#### Lambda

As in Common Lisp `(lambda (p1 p2 ... pn) e)`, in this syntax it should be `(p1:t1, p2:t2, ..., pn:tn) -> e`, where `t1, t2, ..., tn` are type annotation.

#### Call

Given function named `f`, called with arguments `a`, `b`, `c`, should be `f[a,b,c]`.

#### Top-Level

In top-level, there can be multiple programs(expressions) sharing the same env, so `xxx := yyy => zzz` and `xxx : ttt => zzz` can just be `xxx := yyy` and `xxx: ttt`, making `zzz` the next expression. However, abbr. `xxx : ttt = yyy` is not allowed.

#### Typing

Few basic types are provided, like `int` and `float`. Functions like `add[a, b]` where `a, b` are `int` should typed `[int, int] -> int`. Note there is a `unit` type like in ML.

#### Primitive

This Language does not have a stdlib, so it needs to 'borrow' functions from other languages as primitives.

Example: `Addi: "add_i_i" [int, int] -> int` to import function `add_i_i` as `Addi`. This can also be used to import symbols.

#### Exported Name

To export a local symbol to abi, useless currently cuz I haven't done binary generation.

Example: `"fbnc_i":= fbnc`ensures to export name fbnc to elf symbol 'fbnc_i'.