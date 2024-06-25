module Stack_ir_cnv = struct
  module E = Typing.Env
  module A = Syntactics.AST2
  module S = Stack_ir.StkIR

  



end

module Llvm_ir_cnv = struct
  module E = Typing.Env
  module A = Syntactics.AST2
end

module Cish_cnv = struct
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
end
