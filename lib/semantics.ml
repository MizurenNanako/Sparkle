(** type checking *)
module Check = struct
  module A = Syntactics.AST
  module C = Typing.CType
  module E = Typing.Env
  module R = Lexical.Range

  exception TypeError of string * R.t
  exception NotImplanted

  let rec check_expr (env : E.env) (expr : A.expr) : C.t =
    match expr.expr_desc with
    | ArithExpr (op, l, r) ->
      (* due to design, two side should all be int *)
      let t1, t2 = check_expr env l, check_expr env r in
      (match t1, t2 with
       | C.Cint, C.Cint -> C.Cint
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
    | RelExpr (A.OpPeq, l, r) | RelExpr (A.OpPneq, l, r) ->
      (* physical addr always comparable *)
      let t1, t2 = check_expr env l, check_expr env r in
      ignore (t1, t2);
      C.Cint
    | RelExpr (op, l, r) ->
      (* due to design, two side should all be int *)
      let t1, t2 = check_expr env l, check_expr env r in
      if C.eq t1 t2 && C.eq t1 C.Cint
      then C.Cint
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
      (match check_expr env l with
       | C.Cint -> C.Cint
       | _ ->
         raise
         @@ TypeError ("should be int: opand of " ^ A.show_uop op, l.expr_rng))
    | CallExpr (f, p) ->
      let funty = check_expr env f in
      let paramsty = List.map (check_expr env) p in
      (match C.applyfun funty paramsty with
       | Some ty -> ty
       | None ->
         raise
         @@ TypeError
              ( Printf.sprintf
                  "cannot apply (%s) to (%s)"
                  (C.show funty)
                  (paramsty |> List.map C.show |> String.concat ", ")
              , expr.expr_rng ))
    | CondExpr (brlst, el) ->
      List.fold_left
        (fun acc (p, e) ->
          if check_expr env p <> C.Cint
          then raise @@ TypeError ("predicate should have type <int>", p.expr_rng)
          else if check_expr env e = acc
          then acc
          else
            raise
            @@ TypeError
                 ( Printf.sprintf
                     "these branchs should have same type <%s> with last one"
                     (C.show acc)
                 , e.expr_rng ))
        (check_expr env el)
        brlst
    | ListExpr l ->
      (* type erased, still check is valid *)
      List.iter (fun i -> i |> check_expr env |> ignore) l;
      C.Clist
    (* | AssignExpr _ -> raise NotImplanted *)
    | CompoundExpr (l, r) ->
      (match check_expr env l, check_expr env r with
       | C.Cunit, ty -> ty
       | _ -> raise @@ TypeError ("This should have type unit", l.expr_rng))
    | LetinExpr (l, r) ->
      let env =
        List.fold_left
          (fun acc (name, e) ->
            let ty =
              match check_expr acc e with
              | C.Cunit ->
                raise @@ TypeError ("cannot bind <unit> to variable", e.expr_rng)
              | ty -> ty
            in
            E.add_name name ty acc)
          env
          l
      in
      check_expr env r
    | VarExpr name ->
      (match E.get_name name env with
       | Some ty -> ty
       | None -> raise @@ TypeError ("Unknown Name: " ^ name, expr.expr_rng))
    | IntConst _ -> C.Cint
    | StrConst _ -> C.Cbytes
    | UnitConst -> C.Cunit
    | NilConst -> C.Clist
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

  let check_toplevel (env : E.env) (top : A.toplevel) : E.env =
    match top.topl_desc with
    | DeclTop { top_decl_id; top_decl_type } ->
      let ty = check_type_expr top_decl_type in
      (match E.get_local_name top_decl_id env with
       | None -> E.add_name top_decl_id ty env
       | Some ty' ->
         if C.eq ty ty'
         then env
         else
           raise
           @@ TypeError
                ( Printf.sprintf
                    "old decl <%s> and new decl <%s> unmatched"
                    (C.show ty')
                    (C.show ty)
                , top.topl_rng ))
    | ImplVar { impl_var_id; impl_var_val } ->
      let ty = check_expr env impl_var_val in
      let doit () = E.add_name impl_var_id ty env in
      (match E.get_local_name impl_var_id env with
       | None -> doit ()
       | Some ty' ->
         if C.eq ty ty'
         then doit ()
         else
           raise
           @@ TypeError
                ( Printf.sprintf
                    "decl <%s> and impl <%s> unmatched"
                    (C.show ty')
                    (C.show ty)
                , top.topl_rng ))
    | ImplFun
        { impl_fun_id
        ; impl_fun_param
        ; impl_fun_ptype
        ; impl_fun_rtype
        ; impl_fun_body
        } ->
      let paramsty = List.map check_type_expr impl_fun_ptype in
      let retty = check_type_expr impl_fun_rtype in
      let ty = C.Cfun (paramsty, retty) in
      let doit () =
        let env = env |> E.add_name impl_fun_id ty in
        let env' =
          env
          |> E.add_scope impl_fun_id
          |> E.add_pairs
               (impl_fun_ptype
                |> List.map check_type_expr
                |> List.combine impl_fun_param)
        in
        let bodyty = check_expr env' impl_fun_body in
        if bodyty = retty
        then env
        else
          raise
          @@ TypeError
               ( "return type <"
                 ^ C.show retty
                 ^ "> and body type <"
                 ^ C.show bodyty
                 ^ "> not matched."
               , top.topl_rng )
      in
      (match E.get_local_name impl_fun_id env with
       | None -> doit ()
       | Some ty' ->
         if C.eq ty ty'
         then doit ()
         else
           raise
           @@ TypeError
                ( Printf.sprintf
                    "decl <%s> and impl <%s> unmatched"
                    (C.show ty')
                    (C.show ty)
                , top.topl_rng ))
  ;;

  let check_translation_unit (all : A.t) : unit =
    List.fold_left check_toplevel E.empty all |> ignore
  ;;
end

module Transpile = struct end
