(** Concrete AST, all lexical ranges stripped.
    used for node evaluation or high level ir *)
module CAST = struct
  open Sexplib0.Sexp_conv
  open Typing

  type id = string [@@deriving sexp]
  type i64 = int64 [@@deriving sexp]
  type f64 = float [@@deriving sexp]
  type str = string [@@deriving sexp]

  type cc_expr =
    { cc_expr_desc : cc_expr_desc
    ; cc_expr_ty : MType.t
    }
  [@@deriving sexp]

  and cc_expr_desc =
    | CNop (* for decl won't do anything *)
    | CBind of
        { cc_bind_bind : cc_bound_desc
        ; cc_bind_ctx : cc_expr
        }
    (* special global var binding *)
    | CTopBind of { cc_top_bind : cc_bound_desc }
    | CLambda of
        { cc_func_param : (id * MType.t) list
        ; cc_func_body : cc_expr
        }
    | CCond of cc_branch list
    | CCall of
        { cc_call_callee : cc_expr
        ; cc_call_args : cc_expr list
        }
    | CAI64 of i64
    | CAF64 of f64
    | CAStr of str
    | CAId of id

  and cc_bound_desc =
    { cc_bound_name : id
    ; cc_bound_value : cc_expr
    }

  and cc_branch =
    { cc_branch_pred : cc_expr
    ; cc_branch_expr : cc_expr
    }

  type cc_module =
    { cc_module_export : (id * MType.t) list
    ; cc_module_import : (id * MType.t) list
    ; cc_module_expr : cc_expr
    }
  [@@deriving sexp]

  let cc_i64 i = { cc_expr_desc = CAI64 i; cc_expr_ty = Mi64 }
  let cc_f64 f = { cc_expr_desc = CAF64 f; cc_expr_ty = Mf64 }
  let cc_str s = { cc_expr_desc = CAStr s; cc_expr_ty = Mstr }
  let cc_id s ty = { cc_expr_desc = CAId s; cc_expr_ty = ty }
  let cc_bound id expr = { cc_bound_name = id; cc_bound_value = expr }

  let cc_bind binding ctx =
    { cc_expr_desc =
        CBind { cc_bind_bind = binding; cc_bind_ctx = ctx }
    ; cc_expr_ty = ctx.cc_expr_ty
    }
  ;;

  let cc_topbind bound =
    { cc_expr_desc = CTopBind { cc_top_bind = bound }
    ; cc_expr_ty = Munit
    }
  ;;

  let cc_nop = { cc_expr_desc = CNop; cc_expr_ty = Munit }

  let cc_func paramlist body func_ty =
    { cc_expr_desc =
        CLambda { cc_func_param = paramlist; cc_func_body = body }
    ; cc_expr_ty = func_ty
    }
  ;;

  let cc_call callee args ret_ty =
    { cc_expr_desc =
        CCall { cc_call_callee = callee; cc_call_args = args }
    ; cc_expr_ty = ret_ty
    }
  ;;

  let cc_cond brlst cond_ty =
    { cc_expr_desc = CCond brlst; cc_expr_ty = cond_ty }
  ;;

  let cc_module (expr_lst : cc_expr list) env =
    let import_lst =
      List.filter_map
        (function
          | name, (MType.Mimport _ as s) -> Some (name, s)
          | _ -> None)
        env
    in
    let export_lst =
      List.filter_map
        (function
          | name, (MType.Mexport _ as s) -> Some (name, s)
          | _ -> None)
        env
    in
    let rec toplevel_eliminate (l : cc_expr list) =
      match l with
      | [] -> cc_i64 0L
      | { cc_expr_desc = CTopBind e; _ } :: [] ->
        cc_bind e.cc_top_bind (cc_i64 0L)
      | ({ cc_expr_desc = CLambda _; _ } as e) :: [] ->
        cc_bind (cc_bound "_" e) (cc_i64 0L)
      | { cc_expr_desc = CNop; _ } :: [] -> assert false
      | (_ as e) :: [] -> e
      | { cc_expr_desc = CTopBind e; _ } :: tl ->
        cc_bind e.cc_top_bind (toplevel_eliminate tl)
      | hd :: tl ->
        let bound = cc_bound "_" hd in
        cc_bind bound (toplevel_eliminate tl)
    in
    { cc_module_import = import_lst
    ; cc_module_export = export_lst
    ; cc_module_expr = expr_lst |> toplevel_eliminate
    }
  ;;
end
