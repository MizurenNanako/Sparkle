%{
    open Lexical
    open Syntactics.AST
%}

%token <Range.pos> Tbar "|"
%token <Range.pos> Tcolon ":"
%token <Range.pos> Tlb "["
%token <Range.pos> Trb "]"
%token <Range.pos> Tlp "("
%token <Range.pos> Trp ")"
%token <Range.pos> Tlc "{"
%token <Range.pos> Trc "}"
%token <Range.pos> Tcomma ","

%token <Range.pos> Tbind ":="
%token <Range.pos> Tto "->"
%token <Range.pos> Tin "=>"

%token <int64 * Range.t> Ti64
%token <float * Range.t> Tf64
%token <string * Range.t> Tid
%token <string * Range.t> Tstr

%token Teof

%start <expr list> start

%%

start:
| l = expr*; Teof; { l }

expr:
| a = bind_expr; { a }

call_expr:
|   callee = call_expr; "["; 
    param = separated_list(",", call_expr); 
    posR = "]";
{
    {
        expr_desc = CallExpr {
            call_expr_callee = callee;
            call_expr_param = param;
        };
        expr_rng = snd callee.expr_rng, posR;
    }
}
| e = primary_expr { e }

primary_expr:
| i = Ti64; 
{ 
    {
        expr_desc = I64Atom (fst i);
        expr_rng = snd i;
    }
}
| f = Tf64;
{
    {
        expr_desc = F64Atom (fst f);
        expr_rng = snd f;
    }
}
| s = Tstr;
{
    {
        expr_desc = StrAtom (fst s);
        expr_rng = snd s;
    }
}
| i = Tid;
{
    {
        expr_desc = IdAtom (fst i);
        expr_rng = snd i;
    }
}
| "("; e = expr; ")"; { e }

bind_expr:
| a = id; ":="; e1 = expr; "=>"; e2 = lambda_expr;
{
    {
        expr_desc = BindExpr {
            bind_name = fst a;
            bind_value = e1;
            bind_ctx = e2;
        };
        expr_rng = Range.join (snd a) e2.expr_rng;
    }
}
| e = lambda_expr { e }

cond_expr:
| cd = cond;
{
    {
        expr_desc = CondExpr cd;
        expr_rng = cd.cond_rng;
    }
}
| e = call_expr { e }

cond:
| c = cond_;
{
    {
        cond_branch = [ c ];
        cond_rng = Range.join c.branch_pred.expr_rng c.branch_expr.expr_rng;
    }
}
| l = cond; "|"; c = cond_;
{
    {
        cond_branch = c :: l.cond_branch;
        cond_rng = Range.join l.cond_rng c.branch_expr.expr_rng;
    }
}

cond_:
| p = call_expr; "->"; e = call_expr;
{
    {
        branch_pred = p;
        branch_expr = e;
    }
}

lambda_expr:
| posL = "("; p = separated_list(",", param_); ")"; "->"; e = cond_expr;
{
    {
        expr_desc = LambdaExpr {
            lambda_param = p;
            lambda_expr = e;
        };
        expr_rng = posL, snd e.expr_rng
    }
}
| e = cond_expr; { e }

param_:
| a = id; ":"; te = type_expr;
{
    {
        param_item_id = fst a;
        param_item_type = te;
    }
}

type_expr:
| a = id;
{
    {
        type_expr_desc = TIdAtom (fst a);
        type_expr_rng = snd a;
    }
}

id:
| a = Tid; { a }
