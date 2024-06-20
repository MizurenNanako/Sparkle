module AST = struct
  type rng = Lexical.Range.t
  type id = string

  type toplevel =
    { topl_desc : topl_desc
    ; topl_rng : rng
    }

  and topl_desc =
    | DeclVar of
        { decl_var_id : id
        ; decl_var_type : type_expr
        }
    | DeclFun of
        { decl_fun_id : id
        ; decl_fun_ptype : type_expr list
        ; decl_fun_rtype : type_expr
        }
    | ImplVar of
        { impl_var_id : id
        ; impl_var_type : type_expr
        ; impl_var_val : expr
        }
    | ImplFun of
        { impl_fun_id : id
        ; impl_fun_param : id list
        ; impl_fun_ptype : type_expr list
        ; impl_fun_rtype : type_expr
        ; impl_fun_body : expr
        }

  and type_expr =
    { type_expr_desc : type_expr_desc
    ; type_expr_rng : rng
    }

  and type_expr_desc =
    | Tint
    | Tlist
    | Tbytes
    | Tnil
    | Tunit
    | Tfun of type_expr list * type_expr

  and expr =
    { expr_desc : expr_desc
    ; expr_rng : rng
    }

  and expr_desc =
    | ArithExpr of arithop * expr * expr
    | StrOpExpr of strop * expr * expr
    | RelExpr of relop * expr * expr
    | UnaryExpr of uop * expr
    | CallExpr of expr * expr list
    | CondExpr of (expr * expr) list * expr
    | ListExpr of expr list
    | AssignExpr of expr * expr
    | CompoundExpr of expr * expr
    | LetinExpr of (id * expr) list * expr

  and arithop =
    | OpAdd
    | OpSub
    | OpMul
    | OpDiv

  and strop = OpSConcat

  and uop =
    | OpPosi
    | OpNega

  and relop

  type t = toplevel list
end
