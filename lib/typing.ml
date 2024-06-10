module MType = struct
  open Sexplib0.Sexp_conv

  type t =
    | Munit
    | Merr
    | Mi64
    | Mf64
    | Mstr
    | Mbytes of int
    | Msig of t
    | Mlambda of t list * t
    | Mpair of t * t
  [@@deriving sexp_of]

  let of_tid (tid : string) =
    match tid with
    | "unit" -> Munit
    | "i64" | "int" -> Mi64
    | "f64" | "float" -> Mf64
    | "str" | "string" -> Mstr
    | s ->
      (* case of bytes *)
      (match String.starts_with ~prefix:"b" s with
       | true ->
         let maybe_num = String.sub s 1 (String.length s - 1) in
         (match int_of_string_opt maybe_num with
          | Some i -> Mbytes i
          | None -> Merr)
       | false -> Merr)
  ;;

  let eq (t1 : t) (t2 : t) = Stdlib.compare t1 t2 = 0

  let call_on (callee : t) (args : t list) =
    match callee with
    | Mlambda (param, ret) | Msig (Mlambda (param, ret)) ->
      if param = args then Some ret else None
    | _ -> None
  ;;

  let mk_pair t1 t2 = Mpair (t1, t2)

  let car = function
    | Mpair (a, _) -> a
    | _ -> Merr
  ;;

  let cdr = function
    | Mpair (_, b) -> b
    | _ -> Merr
  ;;

  let atom_p = function
    | Mpair _ -> false
    | _ -> true
  ;;

  let null_p = function
    | Munit -> true
    | _ -> false
  ;;
end
