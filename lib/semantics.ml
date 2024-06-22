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
    | StrOpExpr (op, l, r) ->
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
              ("should be int: both sides of " ^ A.show_strop op, expr.expr_rng))
    | RelExpr (op, l, r) ->
      (* due to design, two side should all be int *)
      let t1, t2 = check_expr env l, check_expr env r in
      (match t1, t2 with
       | C.Cint, C.Cint -> C.Cint
       | C.Cint, _ ->
         raise
         @@ TypeError ("should be int: rhs of " ^ A.show_relop op, r.expr_rng)
       | _, C.Cint ->
         raise
         @@ TypeError ("should be int: lhs of " ^ A.show_relop op, l.expr_rng)
       | _ ->
         raise
         @@ TypeError
              ("should be int: both sides of " ^ A.show_relop op, expr.expr_rng))
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
                  "cannot apply type %s to type %s"
                  (C.show funty)
                  (paramsty |> List.map C.show |> String.concat ", ")
              , expr.expr_rng ))
    | CondExpr (brlst, el) ->
      List.fold_left
        (fun acc (p, e) ->
          if check_expr env p <> C.Cint
          then raise @@ TypeError ("predicate should have type int", p.expr_rng)
          else if check_expr env e = acc
          then acc
          else raise @@ TypeError ("branchs should have same type", e.expr_rng))
        (check_expr env el)
        brlst
    | ListExpr l ->
      (* type erased, still check is valid *)
      List.iter (fun i -> i |> check_expr env |> ignore) l;
      C.Clist
    | AssignExpr _ -> raise NotImplanted
    | CompoundExpr (l, r) ->
      (match check_expr env l, check_expr env r with
       | C.Cunit, ty -> ty
       | _ -> raise @@ TypeError ("This should have type unit", l.expr_rng))
    | LetinExpr (l, r) ->
      let env =
        List.fold_left
          (fun acc (name, e) ->
            let ty = check_expr acc e in
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
end
