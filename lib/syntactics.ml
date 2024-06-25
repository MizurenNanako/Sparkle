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
    | OpMod
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
  type id = Typing.Env.id [@@deriving show { with_path = false }]

  module C = Typing.CType

  type toplevel =
    | ImplFunc of
        { impl_func_id : id 
        ; impl_func_param : id list
        (* ; impl_func_pcnt : int *)
        ; impl_func_ptype : C.t list
        ; impl_func_body : expr
        }
    | ImplData of
        { impl_data_id : id
        ; impl_data_value : expr
        ; impl_data_type : C.t
        }
  [@@deriving show { with_path = false }]

  and expr =
    (* arith *)
    | ArithExpr of arithop * expr * expr
    (* | SConcatExpr of expr * expr *)
    | NotExpr of expr
    | NegExpr of expr
    (* call *)
    | CallExpr of expr * expr list
    | PrimExpr of string * expr list
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
    | StrConst of Typing.Env.id (* only an id *)
    | UnitConst
    | NilConst

  and arithop =
    | OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod
    | OpAnd
    | OpOr
    | OpXor
    | OpXnor
    | OpShl
    | OpShr
    | OpLshr
    | OpEq
    | OpLt
    | OpGt
    | OpPeq
    | OpPneq
    | OpNeq
    | OpLeq
    | OpGeq

  let arithop_cnv = function
    | AST.OpAdd -> OpAdd
    | AST.OpSub -> OpSub
    | AST.OpMul -> OpMul
    | AST.OpDiv -> OpDiv
    | AST.OpMod -> OpShr
    | AST.OpAnd -> OpAnd
    | AST.OpOr -> OpOr
    | AST.OpXor -> OpXor
    | AST.OpXnor -> OpXnor
    | AST.OpShl -> OpShl
    | AST.OpShr -> OpShr
    | AST.OpLshr -> OpLshr
  ;;

  let relop_cnv = function
    | AST.OpEq -> OpEq
    | AST.OpLt -> OpLt
    | AST.OpGt -> OpGt
    | AST.OpPeq -> OpPeq
    | AST.OpPneq -> OpPneq
    | AST.OpNeq -> OpNeq
    | AST.OpLeq -> OpLeq
    | AST.OpGeq -> OpGeq
  ;;
end
