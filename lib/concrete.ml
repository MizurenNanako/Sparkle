(** Concrete AST, all lexical ranges stripped. *)
module CAST = struct
  open Sexplib0.Sexp_conv
  open Typing

  type id = string [@@deriving sexp]
  type i64 = int64 [@@deriving sexp]
  type f64 = float [@@deriving sexp]
  type str = string [@@deriving sexp]

  type expr =
    { expr_desc : expr_desc
    ; expr_ty : MType.t
    }
  [@@deriving sexp_of]

  and expr_desc =
    | CScope of
        { scope_bind : bound_desc list
        ; scope_ctx : expr
        }
    | CFunc of func_expr_desc
    | CCond of cond_expr_desc
    | CCall of
        { call_expr_callee : expr
        ; call_expr_param : expr list
        }
    | CAI64 of i64
    | CAF64 of f64
    | CAStr of str
    | CAId of id

  and bound_desc =
    { bound_name : id
    ; bound_value : expr
    }

  and func_expr_desc =
    { func_param : (id * MType.t) list
    ; func_body : expr
    }

  and cond_expr_desc = { cond_branch : branch list }

  and branch =
    { branch_pred : expr
    ; branch_expr : expr
    }
end
