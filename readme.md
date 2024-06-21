# Sparkle Language

This project is being refractoring.

## Brief

Top level only allow function declarations, var decl, func impl, var init. And source include.

<code>`filename</code> to include filename.

Allowed type:

- `int` machine width integer (4 bytes)
- `list` int list construct
- `bytes` sized bytes. object with size and refcnt, pointer on stack.
- `nil` special type
- `unit` empty type
- `(t1, t2, ..., tn) -> rt` function type

primitive binary operator:

- `+ - * /` arithmetic, all `[int, int] -> int`
- `= <> < > <= >=` relop
- `== !=` physical comparison
- `:=` assignment
- `^` bytes concate
- `.` reversed apply, `a.f[]` equals to `f[a]`

primitive unary operator:

- `+ -` positive negative

primitive function:

```m
car: [list] -> int
cdr: [list] -> list
cons: [int, list] -> list
deref: [int] -> bytes
substr: [bytes, int, int] -> bytes
strlen: [bytes] -> int
new: [int] -> bytes
del: [bytes] -> unit
```

operator order:

- `:=` assign
- `= <> < > <= >= == !=` rel
- `^` bytes concate
- `+ -` binary
- `* /`
- `+ -` unary
- `[]` call
- `.` rev apply, `a1.f[a2, a3, ..., an]` equals to `f[a1, a2, ..., an]`

primitive constant:

- `nil` end of every list, implanted as `NULL` in C.

basic syntax of top level:

- `fun_name : [t1, t2, ..., tn] -> ret_type` func decl, param name optional.
- `fun_name : [p1: t1, p2: t2, ...] -> ret_type = expr` func impl
- `var_name : type` var decl
- `var_name : type = value` var init

basic syntax of expr:

- `a bop b` bop is one of binary operator
- `uop a` uop is one of unary operator
- `? pred1 -> expr1 | pred2 -> expr2 | ... -> exprn` condition expr
- `a` expr
- `(a)` expr
- `f[a1, a2, ..., an]` call expr
- `{a1, a2, a3, ..., an}` list construct expr
- `a <- value` assignment, type is `unit`
- `expr1; expr2; ...; exprn` compound expression, all `expri` $0<i<n$ typed `unit`, take type of `exprn`.
- `name1 := value1, name2 := value2, ..., namen := valuen => expr` let-in expr. can only bind value, not functions.

## AST

type expr:
func type, basic type

top level:
var decl, func decl, var init, func impl

constant:
int, string, unit, nil

expr: [with lexical range]
binary expr, unary expr, call expr, cond expr, list expr, 
assign expr, compound expr, let-in expr

binary expr: [inline]
rel expr, arith expr, rev apply expr
