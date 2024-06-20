module CType = struct
  type t =
    | Cint
    | Clist
    | Cbytes
    | Cnil
    | Cunit
    | Cfun of t list * t
end
