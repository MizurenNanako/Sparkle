module AST = struct
  type rng = Lexical.Range.t [@@deriving show { with_path = false }]
  type id = string [@@deriving show { with_path = false }]

  type toplevel =
    { topl_desc : topl_desc
    ; topl_rng : rng
    }
  [@@deriving show { with_path = false }]

  and topl_desc =
    | DeclTop of
        { top_decl_id : id
        ; top_decl_type : type_expr
        }
    | ImplVar of
        { impl_var_id : id
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
    | Tatom of id
    | Tfun of type_expr list * type_expr

  and expr =
    { expr_desc : expr_desc
    ; expr_rng : rng
    }

  and expr_desc =
    | ArithExpr of arithop * expr * expr
    (* | StrOpExpr of strop * expr * expr *)
    | RelExpr of relop * expr * expr
    | UnaryExpr of uop * expr
    | CallExpr of expr * expr list
    | CondExpr of (expr * expr) list * expr
    | ListExpr of expr list
    (* | AssignExpr of expr * expr *)
    | CompoundExpr of expr * expr
    | LetinExpr of (id * expr) list * expr
    | VarExpr of id
    | IntConst of int64
    | StrConst of string
    | UnitConst
    | NilConst

  and arithop =
    | OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpAnd
    | OpOr
    | OpXor
    | OpXnor
    | OpShl
    | OpShr
    | OpLshr

  (* and strop = OpSConcat *)
  and uop =
    | OpPosi
    | OpNega
    | OpNot

  and relop =
    | OpEq
    | OpLt
    | OpGt
    | OpPeq
    | OpPneq
    | OpNeq
    | OpLeq
    | OpGeq

  type t = toplevel list [@@deriving show { with_path = false }]
end

module AST2 = struct
  type id =
    | Named of string
    | Mangled of int
  [@@deriving show { with_path = false }]

  module C = Typing.CType

  type toplevel =
    | ImplFunc of
        { impl_func_param : id list
        ; impl_func_ptype : C.t list
        ; impl_func_body : expr
        }
    | ImplData of
        { impl_data_value : const
        ; impl_data_type : C.t
        }

  and expr =
    { expr_desc : expr_desc
    ; expr_type : C.t
    }

  and expr_desc =
    (* arith *)
    | AddExpr of expr * expr
    | SubExpr of expr * expr
    | MulExpr of expr * expr
    | DivExpr of expr * expr
    | AndExpr of expr * expr
    | OrExpr of expr * expr
    | XorExpr of expr * expr
    | XnorExpr of expr * expr
    | ShlExpr of expr * expr
    | ShrExpr of expr * expr
    | LshrExpr of expr * expr
    | SConcatExpr of expr * expr
    | NotExpr of expr
    | NegExpr of expr
    (* rel *)
    | EqExpr of expr * expr
    | LtExpr of expr * expr
    | GtExpr of expr * expr
    | PeqExpr of expr * expr
    | PneqExpr of expr * expr
    | NeqExpr of expr * expr
    | LeqExpr of expr * expr
    | GeqExpr of expr * expr
    (* call *)
    | CallExpr of expr * expr list
    (* branch *)
    | IfElseExpr of expr * expr * expr
    (* atom *)
    | VarExpr of id
    | ConstExpr of const
    (* stmt *)
    | CompstExpr of expr * expr
    (* scope *)
    | BindExpr of id * expr * expr

  and const =
    | IntConst of int64
    | StrConst of string
    | NilConst
end
