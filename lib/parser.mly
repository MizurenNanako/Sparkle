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
%token <Range.pos> Teq "="
// %token <Range.pos> Tquest "?"
// %token <Range.pos> Tsemi ","

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
| l = top_expr*; Teof; { l }

top_expr:
| a = expr; { a }
| b = decl_expr; { b }
| c = top_bind_expr; { c }
| d = export_expr; { d }

expr:
// | a = bind_expr; { a }
| b = lambda_expr; { b }
| c = local_decl_expr; { c }

export_expr:
| sym = str; ":="; name = id;
{
    {
        expr_desc = ExportExpr {
            export_sym = fst sym;
            export_name = fst name;
        };
        expr_rng = Range.join (snd sym) (snd name);
    }
}

top_bind_expr:
| a = id; ":="; e = expr;
{
    {
        expr_desc = TopBindExpr {
            top_bind_name = fst a;
            top_bind_value = e;
        };
        expr_rng = Range.join (snd a) e.expr_rng;
    }
}

decl_expr:
| a = id; ":"; te = type_expr;
{
    {
        expr_desc = DeclExpr {
            decl_name = fst a;
            decl_type = te;
        };
        expr_rng = Range.join (snd a) te.type_expr_rng;
    }
}
| a = id; ":"; extid = str; te = type_expr;
{
    {
        expr_desc = ExtDeclExpr {
            ext_decl_name = fst a;
            ext_decl_type = te;
            ext_decl_symbol = fst extid;
        };
        expr_rng = Range.join (snd a) te.type_expr_rng;
    }
}

local_decl_expr:
| a = id; ":"; te = type_expr; "=>"; e = local_decl_expr;
{
    {
        expr_desc = LocalDeclExpr {
            local_decl_name = fst a;
            local_decl_type = te;
            local_decl_ctx = e;
        };
        expr_rng = Range.join (snd a) e.expr_rng
    }
}
| a = id; ":"; te = type_expr; "="; e1 = expr; "=>"; e2 = expr;
{
    let e = {
        expr_desc = BindExpr {
            bind_name = fst a;
            bind_value = e1;
            bind_ctx = e2;
        };
        expr_rng = Range.join (snd a) e2.expr_rng;
    } in
    {
        expr_desc = LocalDeclExpr {
            local_decl_name = fst a;
            local_decl_type = te;
            local_decl_ctx = e;
        };
        expr_rng = Range.join (snd a) e.expr_rng
    }
}
| b = bind_expr; { b }

bind_expr:
| a = id; ":="; e1 = expr; "=>"; e2 = expr;
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

call_expr:
|   callee = call_expr; "["; 
    param = separated_list(",", call_expr); 
    p = "]";
{
    {
        expr_desc = CallExpr {
            call_expr_callee = callee;
            call_expr_param = param;
        };
        expr_rng = fst callee.expr_rng, { p with pos_cnum = p.pos_cnum + 1 };
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

cond_expr:
| cd = cond;
{
    {
        expr_desc = CondExpr cd;
        expr_rng = cd.cond_rng;
    }
}
| e = call_expr { e }

cond: c = cond_rev; 
{
    {
        c with
        cond_branch = List.rev c.cond_branch
    }
}

cond_rev:
| c = cond_;
{
    {
        cond_branch = [ c ];
        cond_rng = Range.join c.branch_pred.expr_rng c.branch_expr.expr_rng;
    }
}
| l = cond_rev; "|"; c = cond_;
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
| posL = "("; p = separated_list(",", param_); ")"; 
    "->"; e = expr;
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
| posL = "["; pa = separated_list(",", type_expr); "]"; "->"; ret = type_expr;
{
    {
        type_expr_desc = TLambda (pa, ret);
        type_expr_rng = posL, snd ret.type_expr_rng;
    }
}
| posL = "("; te = type_expr; posR = ")";
{
    {
        te with 
        type_expr_rng = posL, posR;
    }
} 

id:
| a = Tid; { a }

str:
| a = Tstr; { a }
