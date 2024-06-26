module Stack_ir_cnv = struct
  module E = Typing.Env
  module A = Syntactics.AST2
  module S = Stack_ir.StkIR

  let refenv = ref E.empty
  let dictionary : E.CtoR.t ref = ref E.CtoR.empty

  let set_env (env : E.env) =
    refenv := env;
    dictionary := E.get_cid_name_rel env
  ;;

  let get_realname_of_cid (cid : string) = E.CtoR.find cid !dictionary
  let cst_tab : S.data_table = ref []
  let frame_tab : S.frame_table = ref []
  let cst_cnt = ref 0
  let label_cnt = ref 0

  let mk_lbl () =
    let lbl = !label_cnt in
    incr label_cnt;
    "L" ^ string_of_int lbl
  ;;

  let mk_data b =
    (* special global id *)
    let cid = "_byt" ^ string_of_int !cst_cnt in
    incr cst_cnt;
    S.{ segd_symbol = cid; segd_raw = b }, cid
  ;;

  let mk_cst cid b =
    incr cst_cnt;
    S.{ segd_symbol = cid; segd_raw = b }
  ;;

  let strid = Typing.Env.strid

  let rec size_of_type (ty : Typing.CType.t) =
    match ty with
    | Typing.CType.Cint -> 1
    | Typing.CType.Clist -> 1 (* pointer *)
    | Typing.CType.Cbytes -> 1 (* pointer *)
    | Typing.CType.Cunit -> 0 (* not allowed *)
    | Typing.CType.Cfun (_, _) -> 1 (* func pointer *)
    | Typing.CType.Cdecl d -> d |> Typing.CType.unwrap |> size_of_type
  ;;

  let cc_const (c : A.const) =
    match c with
    | A.IntConst i ->
      (* load im number *)
      [ S.IM (S.i64_to_word i) ]
    | A.StrConst b ->
      (* store the assigned symbol and content to cst_tab *)
      let r, cid = mk_data b in
      cst_tab := (cid, r) :: !cst_tab;
      (* load from [cid] *)
      [ S.LOAD_CONST cid ]
    | A.UnitConst ->
      (* nop, for this only happen in valueless call *)
      [ S.NOP ]
    | A.NilConst ->
      (* list nil is actually word 0 *)
      [ S.IM 0l ]
  ;;

  let rec cc_expr (e : A.expr) : S.instruction list =
    match e with
    | A.ArithExpr (op, l, r) -> cc_aexpr op l r
    | A.NotExpr l -> cc_expr l @ [ OP1 S.NOT ]
    | A.NegExpr l -> cc_expr l @ [ OP1 S.NEG ]
    | A.CallExpr (A.VarExpr id, args) ->
      let fname = get_realname_of_cid (strid id) in
      let tyl, rt =
        match E.get_name fname !refenv with
        | Some (Typing.CType.Cfun (tyl, rt))
        | Some (Typing.CType.Cdecl (Typing.CType.Cfun (tyl, rt))) -> tyl, rt
        | _ -> raise @@ Failure "cc_expr"
      in
      (* get the func to be called *)
      let framerec =
        try List.assoc fname !frame_tab with
        | _ ->
          (* then make it *)
          { frame_code = []
          ; frame_retval_size = size_of_type rt
          ; frame_params_sizes = List.map size_of_type tyl
          }
      in
      (* doesn't matter since all ret are word *)
      (* get args *)
      let ccargs = List.concat_map cc_expr (List.rev args) in
      let argssiz = List.fold_left ( + ) 0 framerec.S.frame_params_sizes in
      [ S.PRECALL; ALLOC_RET ] @ ccargs @ [ CALL fname; POPN argssiz ]
    | A.IfElseExpr (p, a, b) ->
      let label_end = mk_lbl () in
      let label_else = mk_lbl () in
      cc_expr p
      @ [ S.JZ label_else ]
      @ cc_expr a
      @ [ S.J label_end; S.LABEL label_else ]
      @ cc_expr b
      @ [ S.LABEL label_end ]
    | A.VarExpr (E.IdTmp _ as x) -> [ S.LOAD_NAME (strid x) ]
    | A.VarExpr (E.IdCst _ as x) -> [ S.LOAD_CONST (strid x) ]
    | A.VarExpr (E.IdArg n) -> [ S.LOAD_ARG n ]
    | A.ConstExpr c -> cc_const c
    | A.CompstExpr (s, e) -> cc_expr s @ cc_expr e
    | A.BindExpr (a, b, c) ->
      [ S.BIND (strid a) ] @ cc_expr b @ cc_expr c @ [ POP ]
    | _ -> raise @@ Failure "func calc is unimplanted"

  and cc_aexpr (aop : A.arithop) (l : A.expr) (r : A.expr) =
    match aop, l, r with
    | A.OpAdd, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.ADD ]
    | A.OpSub, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.SUB ]
    | A.OpMul, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.MUL ]
    | A.OpDiv, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.DIV ]
    | A.OpMod, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.MOD ]
    | A.OpAnd, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.AND ]
    | A.OpOr, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.OR ]
    | A.OpXor, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.XOR ]
    | A.OpXnor, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.XNOR ]
    | A.OpShl, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.SHL ]
    | A.OpShr, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.SHR ]
    | A.OpLshr, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.LSHR ]
    | A.OpEq, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.EQ ]
    | A.OpLt, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.LT ]
    | A.OpGt, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.GT ]
    | A.OpNeq, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.NEQ ]
    | A.OpLeq, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.LEQ ]
    | A.OpGeq, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.GEQ ]
    | A.OpPeq, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.EQ ]
    | A.OpPneq, l, r -> cc_expr r @ cc_expr l @ [ S.OP2 S.NEQ ]
  ;;

  (* this function should construct a frame_record and append it to frame_table *)
  let cc_toplevel (top : A.toplevel) =
    match top with
    | A.ImplFunc
        { impl_func_id
        ; impl_func_param
        ; impl_func_ptype
        ; impl_func_rtype
        ; impl_func_body
        } ->
      ignore impl_func_param;
      let frame_retval_size = size_of_type impl_func_rtype in
      let frame_code =
        if frame_retval_size = 0
        then [ S.FRAME_ENTER ] @ cc_expr impl_func_body @ [ FRAME_EXIT; RET ]
        else
          [ S.FRAME_ENTER ]
          @ cc_expr impl_func_body
          @ [ STORE_RET; FRAME_EXIT; RET ]
      in
      let frame_rec =
        S.
          { frame_params_sizes = List.map size_of_type impl_func_ptype
          ; frame_retval_size
          ; frame_code
          }
      in
      let real_name = get_realname_of_cid (strid impl_func_id) in
      frame_tab := (real_name, frame_rec) :: !frame_tab
    | A.ImplData d ->
      (match d.impl_data_value with
       | A.IntConst i ->
         let i = S.i64_to_word i in
         (* create native endian bytes of word *)
         let b = Bytes.create (Int32.to_int S.size_of_word) in
         Bytes.set_int32_ne b 0 i;
         let cid = strid d.impl_data_id in
         let r = mk_cst cid b in
         cst_tab := (cid, r) :: !cst_tab
       | A.StrConst b ->
         let cid = strid d.impl_data_id in
         let r = mk_cst cid b in
         cst_tab := (cid, r) :: !cst_tab
       | A.NilConst ->
         (* create native endian bytes of word *)
         let b = Bytes.create (Int32.to_int S.size_of_word) in
         Bytes.set_int32_ne b 0 0l;
         let cid = strid d.impl_data_id in
         let r = mk_cst cid b in
         cst_tab := (cid, r) :: !cst_tab
       | A.UnitConst -> assert false (*impossible*))
  ;;
end

(* module Llvm_ir_cnv = struct
   module E = Typing.Env
   module A = Syntactics.AST2
   end *)

(* module Cish_cnv = struct
  module E = Typing.Env
  module A = Syntactics.AST2

  exception NotImplanted of string

  let dirty_constant_table = E.dirty_table.dirty_constant_table

  let transpile_id (i : A.id) =
    match i with
    | E.IdTmp i -> "tmp" ^ string_of_int i
    | E.IdCst i -> "cst" ^ string_of_int i
    | E.IdArg i -> "arg" ^ string_of_int i
  ;;

  let transpile_const (c : A.const) =
    match c with
    | A.IntConst i -> Int64.to_string i
    | A.StrConst id -> transpile_id id
    | A.UnitConst -> "0"
    | A.NilConst -> "0"
  ;;

  let transpile_aop (e : A.arithop) =
    match e with
    | A.OpAdd -> "+"
    | A.OpSub -> "-"
    | A.OpMul -> "*"
    | A.OpDiv -> "/"
    | A.OpMod -> "%"
    | A.OpAnd -> "&&"
    | A.OpOr -> "||"
    | A.OpXor -> raise @@ NotImplanted "Xor"
    | A.OpXnor -> raise @@ NotImplanted "Xnor"
    | A.OpShl -> raise @@ NotImplanted "Shl"
    | A.OpShr -> raise @@ NotImplanted "Shr"
    | A.OpLshr -> raise @@ NotImplanted "Lshr"
    | A.OpEq -> "=="
    | A.OpLt -> "<"
    | A.OpGt -> ">"
    | A.OpPeq -> "=="
    | A.OpPneq -> "!="
    | A.OpNeq -> "!="
    | A.OpLeq -> "<"
    | A.OpGeq -> ">"
  ;;

  let rec transpile_expr (e : A.expr) =
    match e with
    | A.ArithExpr (op, l, r) ->
      transpile_expr l ^ transpile_aop op ^ transpile_expr r
    | A.NotExpr a -> "!" ^ transpile_expr a
    | A.NegExpr a -> "-" ^ transpile_expr a
    | A.CallExpr (f, p) ->
      Printf.sprintf
        "%s(%s)"
        (transpile_expr f)
        (p |> List.map transpile_expr |> String.concat ",")
    | A.PrimExpr (f, p) ->
      Printf.sprintf "%s(%s)" f (p |> List.map transpile_expr |> String.concat ",")
    | A.IfElseExpr (brc, brt, brf) ->
      Printf.sprintf
        "let iftmp = 0; if (%s) {iftmp = %s} else {iftmp = %s}"
        (transpile_expr brc)
        (transpile_expr brt)
        (transpile_expr brf)
    | A.VarExpr a -> transpile_id a
    | A.ConstExpr c -> transpile_const c
    | A.CompstExpr (a, b) -> transpile_expr a ^ ";" ^ transpile_expr b
    | A.BindExpr (a, b, c) ->
      Printf.sprintf
        "%s;\nlet %s=v;{%s}"
        (transpile_expr b)
        (transpile_id a)
        (transpile_expr c)
  ;;

  let transpile_toplevel (t : A.toplevel) =
    match t with
    | A.ImplFunc f ->
      Printf.sprintf
        "%s(%s)\n{\nlet retv = 0; \nv = %s;\nreturn retv;\n}"
        (transpile_id f.impl_func_id)
        (f.impl_func_param |> List.map transpile_id |> String.concat ",")
        (transpile_expr f.impl_func_body)
    | A.ImplData d -> Printf.sprintf "//%s" (transpile_id d.impl_data_id)
  ;;
end *)
