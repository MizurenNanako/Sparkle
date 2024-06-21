%{
    open Lexical.Token
    open Lexical
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
%right UPOS UNEG "not"
%left "."
%nonassoc "{" "}"
%nonassoc "[" "]"
%nonassoc "(" ")"

%%

start:
| l = toplevel*; Teof; { l }

toplevel:
// top decl
| a = id; ":"; ty = type_expr; 
{
    {
        topl_desc = DeclTop {
            top_decl_id = fst a;
            top_decl_type = ty;
        };
        topl_rng = Range.join (snd a), ty.type_expr_rng;
    }
}
// var init
| a = id; ":"; ty = type_expr; "="; v = constant_expr; 
{
    {
        topl_desc = ImplVar {
            impl_var_id = fst a;
            impl_var_type = ty;
            impl_var_val = v;
        };
    }
}
// func impl
| a = id; ":"; "["; pa = paramlist_expr; "]"; 
    "->"; rt = type_expr; "="; b = expr; 
{
    {
        topl_desc = ImplFun {
            impl_fun_id = fst a;
            impl_fun_param = pa;
            impl_fun_ptype = pa;
            impl_fun_rtype = rt;
            impl_fun_body = b;
        };
    }
}

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
| a = Tint; 
{
    {
        expr_desc = IntConst (fst a);
        expr_rng = snd a;
    }
}
| a = Tstr; 
{
    {
        expr_desc = StrConst (fst a);
        expr_rng = snd a;
    }
}
// unit
| posL = "("; posR = ")"; 
{
    {
        expr_desc = UnitConst;
        expr_rng = posL, posR;
    }
}
| a = Tnil; 
{
    {
        expr_desc = NilConst;
        expr_rng = getrng (Tnil a);
    }
}

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
| a = id; {}
| a = constant; { a }

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