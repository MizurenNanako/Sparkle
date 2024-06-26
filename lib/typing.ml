module MType = struct
  open Sexplib0.Sexp_conv

  type t =
    | Munit
    | Merr
    | Mi64
    | Mf64
    | Mstr
    | Mbytes of int
    | Msig of t * bool ref
    | Mlambda of t list * t
    | Mpair of t * t
    (* placeholder *)
    | Mexport of string * t
    | Mimport of string * t
  [@@deriving sexp]

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

  let rec repr (t : t) =
    match t with
    | Munit -> "unit"
    | Merr -> "err"
    | Mi64 -> "i64"
    | Mf64 -> "f64"
    | Mstr -> "str"
    | Mbytes n -> Printf.sprintf "b%i" n
    | Msig (a, b) ->
      (match !b with
       | false -> Printf.sprintf "?<%s>" (repr a)
       | true -> Printf.sprintf "<%s>" (repr a))
    | Mlambda (p, r) ->
      let params = p |> List.map repr |> String.concat ", " in
      Printf.sprintf "[%s] -> %s" params (repr r)
    | Mpair (a, b) -> Printf.sprintf "(%s, %s)" (repr a) (repr b)
    | Mimport (s, a) -> Printf.sprintf "import(\"%s\":%s)" s (repr a)
    | Mexport (s, a) -> Printf.sprintf "export(\"%s\":%s)" s (repr a)
  ;;

  let unwrap t1 =
    match t1 with
    | Msig (a, _) | Mimport (_, a) | a -> a
  ;;

  let eq (t1 : t) (t2 : t) =
    Stdlib.compare (unwrap t1) (unwrap t2) = 0
  ;;

  let call_on (callee : t) (args : t list) =
    match unwrap callee with
    | Mlambda (param, ret) -> if param = args then Some ret else None
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
