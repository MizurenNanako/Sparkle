module Checking = struct
  open Sexplib0.Sexp_conv

  type range = Lexical.Range.t

  exception TypeError of string * range
  exception StructureError of string * range

  module A = Syntactics.AST
  module M = Typing.MType

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

  let rec _expr (env : env) (a : A.expr) : M.t * env =
    match a.expr_desc with
    | F64Atom _ -> M.Mf64, env
    | I64Atom _ -> M.Mi64, env
    | StrAtom _ -> M.Mstr, env
    | IdAtom id ->
      (match List.assoc_opt id env with
       | Some ty -> ty, env
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

  and _expr' env a = _expr env a |> fst

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
    let env = List.append (List.combine param_id param_ty) env in
    let ret_ty = _expr' env lam.lambda_expr in
    M.Mlambda (param_ty, ret_ty), env

  and _bind rng env bnd =
    ignore rng;
    match bnd.bind_name = "_" with
    | false ->
      let ty = _expr' env bnd.bind_value in
      let bond =
        match List.assoc_opt bnd.bind_name env with
        | Some (M.Msig (a, b)) ->
          if M.eq a ty
          then (
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
        (* , bnd.bind_value.expr_rng ) *)
        | _ -> bnd.bind_name, ty
      in
      let env' = bond :: env in
      let rety = _expr' env' bnd.bind_ctx in
      rety, env
    | true -> M.Munit, env

  and _top_bind rng env bnd =
    ignore rng;
    match bnd.top_bind_name = "_" with
    | false ->
      let ty = _expr' env bnd.top_bind_value in
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
      M.Munit, env'
    | true -> M.Munit, env

  and _branch rng env brl =
    ignore rng;
    let rec loop acc l =
      match l with
      | [] -> assert false
      | [ a ] ->
        let brty =
          match a.A.branch_pred.expr_desc with
          | IdAtom "_" -> _expr' env a.A.branch_expr
          | _ ->
            raise
            @@ StructureError
                 ( "the last branch predicate should be _"
                 , a.A.branch_pred.expr_rng )
        in
        (match acc with
         | None -> brty
         | Some ty ->
           if M.eq ty brty
           then ty
           else
             raise
             @@ TypeError
                  ( "branch return types unmatched"
                  , a.A.branch_expr.expr_rng ))
      | a :: tl ->
        let brty =
          match _expr' env a.A.branch_pred with
          | Mi64 -> _expr' env a.A.branch_expr
          | _ ->
            raise
            @@ TypeError
                 ( "branch predicate type should be int"
                 , a.A.branch_pred.expr_rng )
        in
        (match acc with
         | None -> loop (Some brty) tl
         | Some ty ->
           if M.eq ty brty
           then loop (Some ty) tl
           else
             raise
             @@ TypeError
                  ( Printf.sprintf
                      "branch return types unmatched: %s and %s"
                      (Typing.MType.repr ty)
                      (Typing.MType.repr brty)
                  , a.A.branch_expr.expr_rng ))
    in
    loop None brl, env

  and _call rng env c =
    let callee_ty = _expr' env c.call_expr_callee in
    let args_ty = List.map (_expr' env) c.call_expr_param in
    match M.call_on callee_ty args_ty with
    | Some ty -> ty, env
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
      M.Munit, bond :: env

  and _ext_decl rng env d =
    let name = d.ext_decl_name in
    let ty = M.Msig (_type d.ext_decl_type, ref true) in
    match ty with
    | M.Merr ->
      raise
      @@ TypeError
           (Printf.sprintf "Invaild type: \"%s\"" d.ext_decl_name, rng)
    | _ -> M.Munit, (name, ty) :: env

  and _local_decl rng env d =
    let name = d.local_decl_name in
    let ty = M.Msig (_type d.local_decl_type, ref false) in
    match ty with
    | M.Merr -> raise @@ TypeError ("Invaild type", rng)
    | _ ->
      let env' = (name, ty) :: env in
      (* let ty', _ = _expr env' d.local_decl_ctx in *)
      let ty', env' = _expr env' d.local_decl_ctx in
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
          | true -> ty', env)
       | Some _ -> ty', env)

  and _export rng env e =
    match List.assoc_opt e.export_name env with
    | Some entry ->
      (* todo: add to the export list with this entry *)
      ignore entry;
      M.Munit, env
    | None ->
      raise
      @@ TypeError
           ( Printf.sprintf
               "symbol to export is not implanted: %s"
               e.export_name
           , rng )
  ;;

  let check_module (astl : A.expr list) : env =
    let rec loop l env =
      match l with
      | [] -> env
      | a :: tl ->
        let ty, env = _expr env a in
        (match ty with
         | M.Munit -> loop tl env
         | _ ->
           raise
           @@ TypeError
                ( "Type of toplevel expression must be unit"
                , a.expr_rng ))
    in
    loop astl [ "_", M.Munit ]
  ;;
end
