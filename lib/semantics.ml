module Checking = struct
  open Sexplib0.Sexp_conv

  type range = Lexical.Range.t

  exception TypeError of string * range
  exception StructureError of string * range

  module A = Syntactics.AST
  module M = Typing.MType
  open Concrete.CAST

  type id = A.id [@@deriving sexp_of]
  type env = (id * M.t) list [@@deriving sexp_of]

  let rec _type (t : A.type_expr) : M.t =
    match t.type_expr_desc with
    | TIdAtom tid ->
      (match M.of_tid tid with
       | Merr ->
         raise
         @@ TypeError
              ( Printf.sprintf "Unknown type \"%s\"" tid
              , t.type_expr_rng )
       | other -> other)
    | TLambda (param, ret) ->
      let param_ty = List.map _type param in
      let ret_ty = _type ret in
      M.Mlambda (param_ty, ret_ty)
  ;;

  let rec _expr (env : env) (a : A.expr) : M.t * env * cc_expr =
    match a.expr_desc with
    | F64Atom x -> M.Mf64, env, cc_f64 x
    | I64Atom x -> M.Mi64, env, cc_i64 x
    | StrAtom x -> M.Mstr, env, cc_str x
    | IdAtom id ->
      (match List.assoc_opt id env with
       | Some ty -> ty, env, cc_id id ty
       | None ->
         raise
         @@ TypeError
              (Printf.sprintf "Undefined id: %s" id, a.A.expr_rng))
    | BindExpr b -> _bind a.expr_rng env b
    | TopBindExpr b -> _top_bind a.expr_rng env b
    | LambdaExpr l -> _lambda a.expr_rng env l
    | DeclExpr d -> _decl a.expr_rng env d
    | ExtDeclExpr d -> _ext_decl a.expr_rng env d
    | LocalDeclExpr d -> _local_decl a.expr_rng env d
    | CallExpr c -> _call a.expr_rng env c
    | CondExpr c -> _branch a.expr_rng env c.cond_branch
    | ExportExpr e -> _export a.expr_rng env e

  and _expr' env a =
    let a, _, b = _expr env a in
    a, b

  and _lambda rng env lam =
    ignore rng;
    let param_ty =
      lam.lambda_param
      |> List.map (fun entry -> _type entry.A.param_item_type)
    in
    let param_id =
      lam.lambda_param
      |> List.map (fun entry -> entry.A.param_item_id)
    in
    let param_ty_list = List.combine param_id param_ty in
    let env = List.append param_ty_list env in
    let ret_ty, cexpr_body = _expr' env lam.lambda_expr in
    let func_ty = M.Mlambda (param_ty, ret_ty) in
    func_ty, env, cc_func param_ty_list cexpr_body func_ty

  and _bind rng env bnd =
    ignore rng;
    match bnd.bind_name = "_" with
    | false ->
      let ty, cexpr_v = _expr' env bnd.bind_value in
      let bond =
        (* check if declared *)
        match List.assoc_opt bnd.bind_name env with
        | Some (M.Msig (a, b)) ->
          if M.eq a ty
          then (
            (*  mark it implanted *)
            b := true;
            bnd.bind_name, ty)
          else
            raise
            @@ TypeError
                 ( Printf.sprintf
                     "Conflict between signature \"%s\" and bond \
                      type \"%s\""
                     (M.repr a)
                     (M.repr ty)
                 , rng )
        | _ -> bnd.bind_name, ty
      in
      let env' = bond :: env in
      let rety, cexpr_ctx = _expr' env' bnd.bind_ctx in
      rety, env, cc_bind (cc_bound bnd.bind_name cexpr_v) cexpr_ctx
    | true ->
      (* This means bind something to unit
         type inferences should check this. *)
      let ty, cexpr_v = _expr' env bnd.bind_value in
      let rety, cexpr_ctx = _expr' env bnd.bind_ctx in
      let cexpr =
        match M.eq ty M.Munit with
        | true -> cc_bind (cc_bound bnd.bind_name cexpr_v) cexpr_ctx
        | false ->
          raise
          @@ TypeError
               ( Printf.sprintf
                   "Binding non-unit type \"%s\" to unit"
                   (M.repr rety)
               , rng )
      in
      M.Munit, env, cexpr

  and _top_bind rng env bnd =
    ignore rng;
    match bnd.top_bind_name = "_" with
    | false ->
      let ty, cexpr_v = _expr' env bnd.top_bind_value in
      let bond =
        match List.assoc_opt bnd.top_bind_name env with
        | Some (M.Msig (a, b)) ->
          if M.eq a ty
          then (
            b := true;
            bnd.top_bind_name, ty)
          else
            raise
            @@ TypeError
                 ( Printf.sprintf
                     "Conflict between signature \"%s\" and bond \
                      type \"%s\""
                     (M.repr a)
                     (M.repr ty)
                 , rng )
        | _ -> bnd.top_bind_name, ty
      in
      let env' = bond :: env in
      M.Munit, env', cc_topbind (cc_bound bnd.top_bind_name cexpr_v)
    | true ->
      (* this is a side effect evaluation *)
      let ty, cexpr_v = _expr' env bnd.top_bind_value in
      let cexpr =
        match M.eq ty M.Munit with
        | true -> cc_topbind (cc_bound bnd.top_bind_name cexpr_v)
        | false ->
          raise
          @@ TypeError
               ( Printf.sprintf
                   "Binding non-unit type \"%s\" to unit"
                   (M.repr ty)
               , rng )
      in
      M.Munit, env, cexpr

  and _branch rng env brl =
    ignore rng;
    let rec loop (acc : M.t option * cc_branch list) l =
      match l with
      | [] -> assert false
      | [ a ] ->
        let brty, branch =
          match a.A.branch_pred.expr_desc with
          | IdAtom "_" ->
            let act_ty, cexpr_act = _expr' env a.A.branch_expr in
            ( act_ty
            , { cc_branch_pred = cc_i64 1L
              ; cc_branch_expr = cexpr_act
              } )
          | _ ->
            raise
            @@ StructureError
                 ( "the last branch predicate should be _"
                 , a.A.branch_pred.expr_rng )
        in
        (match acc with
         | None, branches -> brty, branch :: branches
         | Some ty, branches ->
           if M.eq ty brty
           then ty, branch :: branches
           else
             raise
             @@ TypeError
                  ( Printf.sprintf
                      "branch return types unmatched: %s and %s"
                      (Typing.MType.repr ty)
                      (Typing.MType.repr brty)
                  , a.A.branch_expr.expr_rng ))
      | a :: tl ->
        let brty, branch =
          match _expr' env a.A.branch_pred with
          | Mi64, mexpr_pred ->
            let act_ty, cexpr_act = _expr' env a.A.branch_expr in
            ( act_ty
            , { cc_branch_pred = mexpr_pred
              ; cc_branch_expr = cexpr_act
              } )
          | _ ->
            raise
            @@ TypeError
                 ( "branch predicate type should be int"
                 , a.A.branch_pred.expr_rng )
        in
        (match acc with
         | None, branches -> loop (Some brty, branch :: branches) tl
         | Some ty, branches ->
           if M.eq ty brty
           then loop (Some ty, branch :: branches) tl
           else
             raise
             @@ TypeError
                  ( Printf.sprintf
                      "branch return types unmatched: %s and %s"
                      (Typing.MType.repr ty)
                      (Typing.MType.repr brty)
                  , a.A.branch_expr.expr_rng ))
    in
    let ty, brlst = loop (None, []) brl in
    ty, env, cc_cond (List.rev brlst) ty

  and _call rng env c =
    let callee_ty, cexpr_callee = _expr' env c.call_expr_callee in
    let args_and_ty = List.map (_expr' env) c.call_expr_param in
    let args_only = List.map snd args_and_ty in
    let args_ty = List.map fst args_and_ty in
    match M.call_on callee_ty args_ty with
    | Some ty -> ty, env, cc_call cexpr_callee args_only ty
    | None ->
      raise
      @@ TypeError
           ( Printf.sprintf
               "Invaild call expression, signature unmatched: sig: \
                %s, called: (%s) -> ???"
               (M.repr callee_ty)
               (args_ty |> List.map M.repr |> String.concat ", ")
           , rng )

  and _decl rng env d =
    let name = d.decl_name in
    let ty = M.Msig (_type d.decl_type, ref false) in
    match ty with
    | M.Merr ->
      raise
      @@ TypeError
           (Printf.sprintf "Invaild type: \"%s\"" d.decl_name, rng)
    | _ ->
      let bond =
        match List.assoc_opt name env with
        | Some (M.Msig (a, { contents = false }) as a') ->
          if M.eq a ty
          then name, ty
          else
            raise
            @@ TypeError
                 ( Printf.sprintf
                     "Signature mismatched: old: %s, new: %s"
                     (M.repr a')
                     (M.repr ty)
                 , rng )
        | Some M.Munit ->
          raise @@ TypeError ("type unit is unbindable", rng)
        | _ -> name, ty
      in
      M.Munit, bond :: env, cc_nop

  and _ext_decl rng env d =
    let name = d.ext_decl_name in
    let ty = M.Mimport (d.ext_decl_symbol, _type d.ext_decl_type) in
    match ty with
    | M.Merr ->
      raise
      @@ TypeError
           (Printf.sprintf "Invaild type: \"%s\"" d.ext_decl_name, rng)
    | _ -> M.Munit, (name, ty) :: env, cc_nop

  and _local_decl rng env d =
    let name = d.local_decl_name in
    let ty = M.Msig (_type d.local_decl_type, ref false) in
    match ty with
    | M.Merr -> raise @@ TypeError ("Invaild type", rng)
    | _ ->
      let env' = (name, ty) :: env in
      (* let ty', _ = _expr env' d.local_decl_ctx in *)
      let ty', env', cexpr_ctx = _expr env' d.local_decl_ctx in
      (* before leave, check the local def is satisfied or not *)
      (match List.assoc_opt name env' with
       | None -> assert false
       | Some (M.Msig (_, b)) ->
         (match !b with
          | false ->
            raise
            @@ TypeError
                 ( Printf.sprintf
                     "Local declaration not satisfied in scope: \
                      \"%s\""
                     name
                 , rng )
          | true -> ty', env, cexpr_ctx)
       | Some _ -> ty', env, cexpr_ctx)

  and _export rng env e =
    match List.assoc_opt e.export_name env with
    | Some entry ->
      ( M.Munit
      , (e.export_name, M.Mexport (e.export_sym, entry)) :: env
      , cc_nop )
    | None ->
      raise
      @@ TypeError
           ( Printf.sprintf
               "symbol to export is not implanted: %s"
               e.export_name
           , rng )
  ;;

  let check_module (astl : A.expr list) : cc_module =
    let rec loop (acc : cc_expr list) env l =
      match l with
      | [] -> acc, env
      | a :: tl ->
        let ty, env, cc = _expr env a in
        (match ty with
         | M.Munit ->
           loop (if cc <> cc_nop then cc :: acc else acc) env tl
         | _ ->
           raise
           @@ TypeError
                ( "Type of toplevel expression must be unit"
                , a.expr_rng ))
    in
    let expr_lst, env = loop [] [] astl in
    let import_lst =
      List.filter_map
        (function
          | name, (M.Mimport _ as s) -> Some (name, s)
          | _ -> None)
        env
    in
    let export_lst =
      List.filter_map
        (function
          | name, (M.Mexport _ as s) -> Some (name, s)
          | _ -> None)
        env
    in
    { cc_module_import = import_lst
    ; cc_module_export = export_lst
    ; cc_module_expr = List.rev expr_lst
    }
  ;;
end
