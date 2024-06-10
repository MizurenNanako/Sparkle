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
    | DeclExpr of decl_expr_desc

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

  and type_expr_desc =
    | TIdAtom of id
    | TLambda of type_expr list * type_expr

  and decl_expr_desc =
    { decl_name : id
    ; decl_type : type_expr
    }
end

module Format = struct
  open AST

  let rec pad out n =
    match n with
    | 0 -> ()
    | n ->
      Printf.fprintf out "  ";
      n |> pred |> pad out
  ;;

  let rec fmt_expr level (out : out_channel) e =
    let { expr_desc; _ } = e in
    match expr_desc with
    | BindExpr e -> fmt_bind level out e
    | CallExpr e -> fmt_call level out e
    | CondExpr e -> fmt_cond level out e
    | LambdaExpr e -> fmt_lambda level out e
    | (IdAtom _ as e)
    | (F64Atom _ as e)
    | (I64Atom _ as e)
    | (StrAtom _ as e) -> fmt_atom out e
    | DeclExpr e -> fmt_decl out e

  and fmt_atom out a =
    let open Printf in
    match a with
    | IdAtom i -> fprintf out "%s" i
    | I64Atom i -> fprintf out "%s" (Int64.to_string i)
    | F64Atom i -> fprintf out "%f" i
    | StrAtom i -> fprintf out "\"%s\"" (String.escaped i)
    | _ -> assert false

  and fmt_bind level out e =
    let { bind_name; bind_value; bind_ctx } = e in
    Printf.fprintf
      out
      "%s := %a\n%a=> %a"
      bind_name
      (fmt_expr level)
      bind_value
      pad
      level
      (fmt_expr level)
      bind_ctx

  and fmt_call level out e =
    let { call_expr_callee; call_expr_param } = e in
    Printf.fprintf
      out
      "%a[%a]"
      (fmt_expr level)
      call_expr_callee
      (fmt_args level)
      call_expr_param

  and fmt_args level out e =
    match e with
    | [] -> ()
    | [ a ] -> Printf.fprintf out "%a" (fmt_expr level) a
    | a :: tl ->
      Printf.fprintf out "%a, " (fmt_expr level) a;
      fmt_args level out tl

  and fmt_cond level out e =
    let { cond_branch; _ } = e in
    Printf.fprintf
      out
      "\n%a  %a"
      pad
      level
      (fmt_branches level)
      cond_branch

  and fmt_branches level out bl =
    match bl with
    | [] -> ()
    | [ { branch_pred; branch_expr } ] ->
      Printf.fprintf
        out
        "%a -> %a"
        (fmt_expr level)
        branch_pred
        (fmt_expr level)
        branch_expr
    | { branch_pred; branch_expr } :: tl ->
      Printf.fprintf
        out
        "%a -> %a\n%a| "
        (fmt_expr level)
        branch_pred
        (fmt_expr level)
        branch_expr
        pad
        level;
      fmt_branches level out tl

  and fmt_lambda level out e =
    let { lambda_param; lambda_expr } = e in
    Printf.fprintf
      out
      "(%a) -> %a"
      fmt_param
      lambda_param
      (fmt_expr (level + 1))
      lambda_expr

  and fmt_param out pl =
    match pl with
    | [] -> ()
    | [ { param_item_id; param_item_type } ] ->
      Printf.fprintf
        out
        "%s: %a"
        param_item_id
        fmt_type_expr
        param_item_type
    | { param_item_id; param_item_type } :: tl ->
      Printf.fprintf
        out
        "%s: %a, "
        param_item_id
        fmt_type_expr
        param_item_type;
      fmt_param out tl

  and fmt_type_expr out ty =
    let { type_expr_desc; _ } = ty in
    match type_expr_desc with
    | TIdAtom s -> Printf.fprintf out "%s" s
    | TLambda (param, ret) ->
      Printf.fprintf
        out
        "(%a) -> %a"
        fmt_type_list
        param
        fmt_type_expr
        ret

  and fmt_type_list out tyl =
    match tyl with
    | [] -> ()
    | [ a ] -> Printf.fprintf out "%a" fmt_type_expr a
    | a :: tl ->
      Printf.fprintf out "%a, " fmt_type_expr a;
      fmt_type_list out tl

  and fmt_decl out e =
    let { decl_name; decl_type } = e in
    Printf.fprintf out "%s: %a\n" decl_name fmt_type_expr decl_type
  ;;

  let rec fmt out (a : expr list) =
    match a with
    | [] -> ()
    | a :: tl ->
      Printf.fprintf out "# entry\n%a\n" (fmt_expr 0) a;
      fmt out tl
  ;;
end
