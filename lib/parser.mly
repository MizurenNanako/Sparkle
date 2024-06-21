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
%token <pos> Tlet ":="
%token <pos> Tassign "<-"
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
%type <expr> expr call_expr

%right "<-"
%right ":="
%left "and" "or" "xor" "xnor"
%left "=" "<>" "<" ">" "<=" ">=" "==" "!="
%left "^"
%left "shl" "shr" "lshr"
%left "+" "-"
%left "*" "/"
%right UPOS UNEG "not"
%left "."
%nonassoc "[" // call

%%

start:
| l = toplevel*; Teof; { l }

// topftype:
// | id; ":"; ty = type_expr; { ty }
// | ty = type_expr; { ty }

toplevel:
// top decl
| a = id; ":"; ty = type_expr; 
{
    {
        topl_desc = DeclTop {
            top_decl_id = fst a;
            top_decl_type = ty;
        };
        topl_rng = Range.join (snd a) ty.type_expr_rng;
    }
}
// | a = id; ":"; posL = "["; te = topftype*; "]"; "->"; rt = type_expr;
// {
//     {
//         topl_desc = DeclTop {
//             top_decl_id = fst a;
//             top_decl_type = {
//                 type_expr_desc = Tfun (te, rt);
//                 type_expr_rng = posL, snd rt.type_expr_rng;
//             }
//         };
//         topl_rng = Range.join (snd a) rt.type_expr_rng;
//     }
// }
// var init
| a = id; ":"; ty = type_expr; ":="; v = constant; 
{
    {
        topl_desc = ImplVar {
            impl_var_id = fst a;
            impl_var_type = ty;
            impl_var_val = v;
        };
        topl_rng = Range.join (snd a) v.expr_rng;
    }
}
// func impl
| a = id; ":"; "["; pa = paramlist_expr; "]"; 
    "->"; rt = type_expr; ":="; b = expr; 
{
    {
        topl_desc = ImplFun {
            impl_fun_id = fst a;
            impl_fun_param = fst pa;
            impl_fun_ptype = snd pa;
            impl_fun_rtype = rt;
            impl_fun_body = b;
        };
        topl_rng = Range.join (snd a) b.expr_rng;
    }
}

paramlist_expr:
| r = paramlist_expr_; { let a, b = r in List.rev a, List.rev b }

paramlist_expr_:
| a = id; ":"; te = type_expr; { [fst a], [te] }
| l = paramlist_expr_; ","; a = id; ":"; te = type_expr; 
{
    let la, lb = l in
    (fst a) :: la, te :: lb
}

type_expr:
// function type
| posL = "["; l = type_expr*; "]"; "->"; rt = type_expr; 
{
    {
        type_expr_desc = Tfun (l, rt);
        type_expr_rng = posL, snd rt.type_expr_rng;
    }
}
// var type
| tid = id; 
{
    {
        type_expr_desc = Tatom (fst tid);
        type_expr_rng = snd tid;
    }
}
| "("; te = type_expr; ")"; { te }

constant:
| a = Tint; { {expr_desc = IntConst (fst a); expr_rng = snd a} }
| a = Tstr; { {expr_desc = StrConst (fst a); expr_rng = snd a} }
// unit
| posL = "("; posR = ")"; { {expr_desc = UnitConst; expr_rng = posL, posR} }
| a = Tnil; { {expr_desc = NilConst; expr_rng = getrng (Tnil a)} }

primary_expr:
| posL = "("; a = expr; posR = ")"; { {a with expr_rng = posL, posR} }

| a = expr; "+"; b = expr; { {expr_desc = ArithExpr (OpAdd, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "-"; b = expr; { {expr_desc = ArithExpr (OpSub, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "*"; b = expr; { {expr_desc = ArithExpr (OpMul, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "/"; b = expr; { {expr_desc = ArithExpr (OpDiv, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "shl"; b = expr; { {expr_desc = ArithExpr (OpShl, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "shr"; b = expr; { {expr_desc = ArithExpr (OpShr, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "lshr"; b = expr; { {expr_desc = ArithExpr (OpLshr, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }

| a = expr; "^"; b = expr; { {expr_desc = StrOpExpr (OpSConcat, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "<-"; b = expr; { {expr_desc = AssignExpr (a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }

| a = expr; "="; b = expr; { {expr_desc = RelExpr (OpEq, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "<"; b = expr; { {expr_desc = RelExpr (OpLt, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; ">"; b = expr; { {expr_desc = RelExpr (OpGt, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "=="; b = expr; { {expr_desc = RelExpr (OpPeq, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "!="; b = expr; { {expr_desc = RelExpr (OpPneq, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "<>"; b = expr; { {expr_desc = RelExpr (OpNeq, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "<="; b = expr; { {expr_desc = RelExpr (OpLeq, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; ">="; b = expr; { {expr_desc = RelExpr (OpGeq, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }

| a = expr; "and"; b = expr; { {expr_desc = ArithExpr (OpAnd, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "or"; b = expr; { {expr_desc = ArithExpr (OpOr, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "xor"; b = expr; { {expr_desc = ArithExpr (OpXor, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }
| a = expr; "xnor"; b = expr; { {expr_desc = ArithExpr (OpXnor, a, b); expr_rng = Range.join a.expr_rng b.expr_rng} }

| posL = "not"; b = expr; { {expr_desc = UnaryExpr (OpNot, b); expr_rng = posL, (snd b.expr_rng)} }
| posL = "+"; b = expr; %prec UPOS { {expr_desc = UnaryExpr (OpPosi, b); expr_rng = posL, (snd b.expr_rng)} }
| posL = "-"; b = expr; %prec UNEG { {expr_desc = UnaryExpr (OpNega, b); expr_rng = posL, (snd b.expr_rng)} }

| posL = "{"; l = separated_list(",", expr); posR = "}"; 
{
    {
        expr_desc = ListExpr l;
        expr_rng = posL, posR;
    }
}
| a = expr; "."; c = call_expr 
{
    match c with
    | {expr_desc = CallExpr (fn, args); _} ->
        {
            expr_desc = CallExpr (fn, a :: args);
            expr_rng = Range.join a.expr_rng c.expr_rng;
        }
    | _ -> assert false
}
| a = id; { {expr_desc = VarExpr (fst a); expr_rng = snd a} }
| a = constant; { a }

expr:
| a = letin_expr; { a }

// letin < compound < cond < call

letin_expr:
| l = letbind; "=>"; b = compound_expr;
{
    {
        expr_desc = LetinExpr (List.rev (fst l), b);
        expr_rng = Range.join (snd l) b.expr_rng;
    }
}
| b = compound_expr; { b }

letbind:
| l = letbind; ","; a = id; ":="; b = expr; { (fst a, b) :: fst l, snd l }
| a = id; ":="; b = expr; { [fst a, b], snd a }

compound_expr:
| a = compound_expr; ";"; b = cond_expr; 
{
    {
        expr_desc = CompoundExpr (a, b);
        expr_rng = Range.join a.expr_rng b.expr_rng;
    }
}
| b = cond_expr; { b }

cond_expr:
| posL = "?"; l = separated_nonempty_list("|", cond_br); "->"; el = letin_expr;
{
    {
        expr_desc = CondExpr (l, el);
        expr_rng = posL, snd el.expr_rng;
    }
}
| a = call_expr; { a }

cond_br:
| p = expr; "->"; e = expr; { (p, e) }

call_expr:
| a = call_expr; "["; l = separated_list(",", primary_expr); posR = "]";
{
    {
        expr_desc = CallExpr (a, l);
        expr_rng = fst a.expr_rng, posR;
    }
}
| b = primary_expr; { b }

id: a = Tid; { a }