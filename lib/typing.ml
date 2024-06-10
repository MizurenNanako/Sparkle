module MType = struct
  type t =
    | Munit
    | Merr
    | Mi64
    | Mf64
    | Mstr
    | Mbytes of int
    | Mlambda of t list * t
    | Mpair of t * t

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
