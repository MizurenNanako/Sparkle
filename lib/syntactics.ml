open Lexical

module AST = struct
  open Sexplib0.Sexp_conv

  type range = Range.t [@@deriving sexp_of]
  type id = string [@@deriving sexp_of]
  type i64 = int64 [@@deriving sexp_of]
  type f64 = float [@@deriving sexp_of]
  type str = string [@@deriving sexp_of]

  type expr =
    { expr_desc : expr_desc
    ; expr_rng : range
    }
  [@@deriving sexp_of]

  and expr_desc =
    | BindExpr of bind_expr_desc
    | LambdaExpr of lambda_expr_desc
    | CondExpr of cond_expr_desc
    | CallExpr of call_expr_desc
    | I64Atom of i64
    | F64Atom of f64
    | StrAtom of str
    | IdAtom of id

  and call_expr_desc =
    { call_expr_callee : expr
    ; call_expr_param : expr list
    }

  and bind_expr_desc =
    { bind_name : id
    ; bind_value : expr
    ; bind_ctx : expr
    }

  and lambda_expr_desc =
    { lambda_param : param_item list
    ; lambda_expr : expr
    }

  and param_item =
    { param_item_id : id
    ; param_item_type : type_expr
    }

  and cond_expr_desc =
    { cond_branch : branch list
    ; cond_rng : range
    }

  and branch =
    { branch_pred : expr
    ; branch_expr : expr
    }

  and type_expr =
    { type_expr_desc : type_expr_desc
    ; type_expr_rng : range
    }

  and type_expr_desc = TIdAtom of id
end
