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
%token <pos> Tsemi ";"
%token <pos> Teq "="
%token <pos> Tlt "<"
%token <pos> Tgt ">"
%token <pos> Tassign ":="
%token <pos> Tpeq "=="
%token <pos> Tpneq "!="
%token <pos> Tneq "<>"
%token <pos> Tleq "<="
%token <pos> Tgeq ">="
%token <pos> Tinduce "=>"
%token <pos> Tto "->"
%token <pos> Tor "or"
%token <pos> Tnil "nil"
%token <pos> Tand "and"
%token <pos> Tnot "not"
%token <pos> Txor "xor"
%token <pos> Tshl "shl"
%token <pos> Tshr "shr"
%token <pos> Tlshr "lshr"
%token <pos> Txnor "xnor"

%token Teof

%start <t> start

%left ":="
%left "and" "or" "xor" "xnor"
%left "=" "<>" "<" ">" "<=" ">=" "==" "!="
%left "^"
%left "shl" "shr" "lshr"
%left "+" "-"
%left "*" "/"
%left UPOS UNEG "not"
%left "."
%nonassoc "{" "}"
%nonassoc "[" "]"
%nonassoc "(" ")"

%%

start:
| l = toplevel*; Teof; { l }

toplevel:
// top decl
| a = id; ":"; ty = type_expr; {}
// var init
| a = id; ":"; ty = type_expr; "="; v = constant; {}
// func impl
| a = id; ":"; "["; pa = paramlist_expr; "]"; 
    "->"; rt = type_expr; "="; b = expr; {}

paramlist_expr:
| a = id; ":"; te = type_expr; {}

type_expr:
// function type
| "["; l = param_type_expr*; "]"; "->"; rt = type_expr; {}
// var type
| tid = id; {}
| "("; te = type_expr; ")"; { te }

param_type_expr:
| a = id?; ":"; te = type_expr; {}

constant:
| a = Tint; {}
| a = Tstr; {}
// unit
| posL = "("; posR = ")"; {}
| a = Tnil; {}

primary_expr:
| posL = "("; a = expr; posR = ")"; { a }

| a = expr; "+"; b = expr; {}
| a = expr; "-"; b = expr; {}
| a = expr; "*"; b = expr; {}
| a = expr; "/"; b = expr; {}
| a = expr; "shl"; b = Tint; {}
| a = expr; "shr"; b = Tint; {}
| a = expr; "lshr"; b = Tint; {}

| a = expr; "^"; b = expr; {}
| a = expr; ":="; b = expr; {}

| a = expr; "="; b = expr; {}
| a = expr; "<"; b = expr; {}
| a = expr; ">"; b = expr; {}
| a = expr; "=="; b = expr; {}
| a = expr; "!="; b = expr; {}
| a = expr; "<>"; b = expr; {}
| a = expr; "<="; b = expr; {}
| a = expr; ">="; b = expr; {}

| a = expr; "and"; b = expr; {}
| a = expr; "or"; b = expr; {}
| a = expr; "xor"; b = expr; {}
| a = expr; "xnor"; b = expr; {}

| "not"; b = expr; {}
| "+"; b = expr; %prec UPOS {}
| "-"; b = expr; %prec UNEG {}

| posL = "{"; l = separated_list(",", expr); posR = "}"; {}
| a = expr; "."; c = call_expr {}
| a = constant; {}
| a = id; {}

expr:
| a = letin_expr; { a }

// letin < compound < cond < call

letin_expr:
| a = id; "="; b = compound_expr; {}
| b = compound_expr; { b }

compound_expr:
| a = compound_expr; ";"; b = cond_expr; {}
| b = cond_expr; { b }

cond_expr:
| posL = "?"; l = separated_list("|", cond_br); "=>"; el = letin_expr; {}
| a = call_expr; { a }

cond_br:
| p = expr; "=>"; e = expr; {}

call_expr:
| a = call_expr; "["; separated_list(",", expr); "]"; {}
| b = primary_expr; { b }

id: a = Tid; { a }