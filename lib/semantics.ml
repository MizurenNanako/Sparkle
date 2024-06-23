(** type checking *)
module Check = struct
  module A = Syntactics.AST
  module X = Syntactics.AST2
  module C = Typing.CType
  module E = Typing.Env
  module R = Lexical.Range

  exception TypeError of string * R.t
  exception NotImplanted

  let rec check_expr (env : E.env) (expr : A.expr) : C.t * X.expr =
    match expr.expr_desc with
    | ArithExpr (op, l, r) ->
      (* due to design, two side should all be int *)
      let (t1, c1), (t2, c2) = check_expr env l, check_expr env r in
      (match t1, t2 with
       | C.Cint, C.Cint -> C.Cint, X.ArithExpr (X.arithop_cnv op, c1, c2)
       | C.Cint, _ ->
         raise
         @@ TypeError ("should be int: rhs of " ^ A.show_arithop op, r.expr_rng)
       | _, C.Cint ->
         raise
         @@ TypeError ("should be int: lhs of " ^ A.show_arithop op, l.expr_rng)
       | _ ->
         raise
         @@ TypeError
              ("should be int: both sides of " ^ A.show_arithop op, expr.expr_rng))
    (* | StrOpExpr (op, l, r) ->
      let t1, t2 = check_expr env l, check_expr env r in
      (match t1, t2 with
       | C.Cbytes, C.Cbytes -> C.Cbytes
       | C.Cbytes, _ ->
         raise
         @@ TypeError ("should be int: rhs of " ^ A.show_strop op, r.expr_rng)
       | _, C.Cbytes ->
         raise
         @@ TypeError ("should be int: lhs of " ^ A.show_strop op, l.expr_rng)
       | _ ->
         raise
         @@ TypeError
              ("should be int: both sides of " ^ A.show_strop op, expr.expr_rng)) *)
    | RelExpr ((A.OpPeq as op), l, r) | RelExpr ((A.OpPneq as op), l, r) ->
      (* physical addr always comparable *)
      let (t1, c1), (t2, c2) = check_expr env l, check_expr env r in
      ignore (t1, t2);
      C.Cint, X.ArithExpr (X.relop_cnv op, c1, c2)
    | RelExpr (op, l, r) ->
      (* due to design, two side should all be int *)
      let (t1, c1), (t2, c2) = check_expr env l, check_expr env r in
      if C.eq t1 t2 && C.eq t1 C.Cint
      then C.Cint, X.ArithExpr (X.relop_cnv op, c1, c2)
      else
        raise
        @@ TypeError
             ( Printf.sprintf
                 "type %s and %s are uncomparable for %s"
                 (C.show t1)
                 (C.show t2)
                 (A.show_relop op)
             , expr.expr_rng )
    | UnaryExpr (op, l) ->
      let ty, c = check_expr env l in
      (match ty with
       | C.Cint ->
         (match op with
          | OpPosi -> C.Cint, c
          | OpNega -> C.Cint, X.NegExpr c
          | OpNot -> C.Cint, X.NotExpr c)
       | _ ->
         raise
         @@ TypeError ("should be int: opand of " ^ A.show_uop op, l.expr_rng))
    | CallExpr (f, p) ->
      let funty, c1 = check_expr env f in
      let paramsty = List.map (check_expr env) p in
      let paramsty, cl = List.map fst paramsty, List.map snd paramsty in
      (match C.applyfun funty paramsty with
       | Some ty -> ty, X.CallExpr (c1, cl)
       | None ->
         raise
         @@ TypeError
              ( Printf.sprintf
                  "cannot apply (%s) to (%s)"
                  (C.show funty)
                  (paramsty |> List.map C.show |> String.concat ", ")
              , expr.expr_rng ))
    | CondExpr (brlst, el) -> check_cond_expr env brlst el
    | ListExpr l ->
      (* type erased, still check is valid *)
      let cl = List.map (fun i -> i |> check_expr env |> snd) l in
      ( C.Clist
      , List.fold_right
          (fun e acc -> X.PrimExpr ("cons", [ e; acc ]))
          cl
          (X.ConstExpr X.NilConst) )
    (* | AssignExpr _ -> raise NotImplanted *)
    | CompoundExpr (l, r) ->
      (match check_expr env l, check_expr env r with
       | (C.Cunit, c1), (ty, c2) -> ty, X.CompstExpr (c1, c2)
       | _ -> raise @@ TypeError ("This should have type unit", l.expr_rng))
    | LetinExpr (l, r) -> check_letin_expr env l r
    | VarExpr name ->
      (match E.get_name' name env with
       | Some (ty, id) -> ty, X.VarExpr id
       | None -> raise @@ TypeError ("Unknown Name: " ^ name, expr.expr_rng))
    | IntConst a -> C.Cint, X.ConstExpr (X.IntConst a)
    | StrConst a -> C.Cbytes, X.ConstExpr (X.StrConst (Bytes.of_string a))
    | UnitConst -> C.Cunit, X.ConstExpr X.UnitConst
    | NilConst -> C.Clist, X.ConstExpr X.NilConst

  (* checks and transform branches to if-else chain *)
  and check_cond_expr env (brlst : (A.expr * A.expr) list) (el : A.expr) =
    (* we check the branches in reversed order *)
    let rec r (tl : (A.expr * A.expr) list) : C.t * X.expr =
      match tl with
      | [] -> check_expr env el
      | (a, b) :: tl ->
        let ta, ca = check_expr env a in
        let tb, cb = check_expr env b in
        let tr, cr = r tl in
        if ta <> C.Cint
        then raise @@ TypeError ("predicate should have type int", a.expr_rng)
        else if tb <> tr
        then
          raise
          @@ TypeError
               ( Printf.sprintf
                   "branch type got <%s>, expected <%s>"
                   (C.show tb)
                   (C.show tr)
               , b.expr_rng )
        else tr, X.IfElseExpr (ca, cb, cr)
    in
    r brlst

  and check_letin_expr (env : E.env) (l : (string * A.expr) list) (rr : A.expr)
    : C.t * X.expr
    =
    let rec r (env : E.env) (tl : (string * A.expr) list) =
      match tl with
      | [] -> check_expr env rr
      | (name, e) :: tl ->
        let ty, cc = check_expr env e in
        let env', id = E.add_name' name ty env in
        let tyy, ccc = r env' tl in
        tyy, BindExpr (id, cc, ccc)
    in
    r env l
  ;;

  let rec check_type_expr (te : A.type_expr) : C.t =
    match te.type_expr_desc with
    | Tatom a ->
      (match C.ctype_of_name a with
       | Some ty -> ty
       | None -> raise @@ TypeError ("unknown type name: " ^ a, te.type_expr_rng))
    | Tfun (p, r) ->
      let p = List.map check_type_expr p in
      let r = check_type_expr r in
      C.Cfun (p, r)
  ;;

  let _match_and_doit the_id the_ty the_env the_rng the_doit =
    match E.is_local_name_type_eq' the_id the_ty the_env with
    | Some (true, C.Cdecl _) ->
      (* allow old sig *)
      the_doit the_env
    | Some (true, _) ->
      raise @@ TypeError ("name redefined", the_rng)
    | Some (false, ty') ->
      raise
      @@ TypeError
           ( Printf.sprintf
               "last decl <%s> and this <%s> unmatched"
               (C.show ty')
               (C.show the_ty)
           , the_rng )
    | _ -> the_doit the_env
  ;;

  let check_toplevel (env : E.env) (top : A.toplevel) : E.env * X.toplevel option =
    match top.topl_desc with
    | A.DeclTop { top_decl_id; top_decl_type } ->
      let ty = C.Cdecl (check_type_expr top_decl_type) in
      let doit env =
        let env = E.add_cname top_decl_id ty env in
        env, None
      in
      _match_and_doit top_decl_id ty env top.topl_rng doit
    | A.ImplVar { impl_var_id; impl_var_val } ->
      (* get the type and code *)
      let ty, cc = check_expr env impl_var_val in
      let doit env =
        (* push it to const entry *)
        let env, id = E.add_cname' impl_var_id ty env in
        ( env
        , Some
            (X.ImplData
               { impl_data_value = cc; impl_data_type = ty; impl_data_id = id }) )
      in
      _match_and_doit impl_var_id ty env top.topl_rng doit
    | A.ImplFun
        { impl_fun_id
        ; impl_fun_param
        ; impl_fun_ptype
        ; impl_fun_rtype
        ; impl_fun_body
        } ->
      let retty = check_type_expr impl_fun_rtype in
      let paramty = List.map check_type_expr impl_fun_ptype in
      let ty = C.Cfun (paramty, retty) in
      let doit env =
        (* construct const label *)
        let env, id = E.add_cname' impl_fun_id ty env in
        (* construct param list *)
        let pairs = List.combine impl_fun_param paramty in
        (* construct inner env, add scope barrier *)
        let env', arg_ids = env |> E.add_scope impl_fun_id |> E.add_args pairs in
        let bodyty, cc = check_expr env' impl_fun_body in
        if C.neq bodyty retty
        then
          raise
          @@ TypeError
               ( Printf.sprintf
                   "funcbody has type <%s>, expected <%s>"
                   (C.show bodyty)
                   (C.show retty)
               , impl_fun_body.expr_rng )
        else
          ( env
          , Some
              (X.ImplFunc
                 { impl_func_param = arg_ids
                 ; impl_func_ptype = paramty
                 ; impl_func_body = cc
                 ; impl_func_id = id
                 }) )
      in
      _match_and_doit impl_fun_id ty env top.topl_rng doit
  ;;

  let check_translation_unit (all : A.toplevel list) : X.toplevel list * E.env =
    let rec r (acc : X.toplevel list) (env : E.env) (tl : A.toplevel list) =
      match tl with
      | [] -> List.rev acc, env
      | a :: tl ->
        let env, cc = check_toplevel env a in
        (match cc with
         | Some cc -> r (cc :: acc) env tl
         | None -> r acc env tl)
    in
    (* discard all decl *)
    r [] E.empty all
  ;;
end

module Transpile = struct end
