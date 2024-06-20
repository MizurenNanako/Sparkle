%{
    open Lexical.Token
    open Syntactics.AST
%}

%token <string * range> Tstr
%token <string * range> Tid
%token <int64 * range> Tint
%token <pos> Tlp "("
%token <pos> Trp ")"
%token <pos> Tlb "["
%token <pos> Trb "]"
%token <pos> Tlc "{"
%token <pos> Trc "}"
%token <pos> Tcomma ","
%token <pos> Tbar "|"
%token <pos> Tquestion "?"
%token <pos> Tadd "+"
%token <pos> Tsub "-"
%token <pos> Tmul "*"
%token <pos> Tdiv "/"
%token <pos> Tsup "^"
%token <pos> Tdot "."
%token <pos> Tcolon ":"
%token <pos> Teq "="
%token <pos> Tlt "<"
%token <pos> Tgt ">"
%token <pos> Tpeq "=="
%token <pos> Tpneq "!="
%token <pos> Tneq "<>"
%token <pos> Tleq "<="
%token <pos> Tgeq ">="
%token <pos> Tinduce "=>"
%token <pos> Tto "->"
%token <pos> Tnil "nil"
%token Teof

%start <t> start

%%

start:
| l = toplevel*; Teof; { l }

toplevel:
// var decl
| a = id; ":"; ty = type_expr; "="; v = constant; {}

id: a = Tid; { a }
str: a = Tstr; { a }