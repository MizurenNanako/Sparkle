module CType = struct
  type t =
    | Cint
    | Clist
    | Cbytes
    | Cunit
    | Cfun of t list * t
    | Cdecl of t

  type 'a typed = 'a * t

  let unwrap (a : t) : t =
    match a with
    | Cdecl a -> a
    | a -> a
  ;;

  let rec show t =
    match t with
    | Cint -> "int"
    | Clist -> "list"
    | Cbytes -> "bytes"
    | Cunit -> "unit"
    | Cfun (l, r) ->
      Printf.sprintf
        "(%s) -> %s"
        (l |> List.map show |> String.concat ", ")
        (show r)
    | Cdecl t -> show t
  ;;

  let ctype_of_name name : t option =
    match name with
    | "int" | "i32" | "i" -> Some Cint
    | "list" | "lst" -> Some Clist
    | "unit" | "_" -> Some Cunit
    | "bytes" | "ptr" | "b" -> Some Cbytes
    | _ -> None
  ;;

  let eq (a : t) (b : t) : bool = unwrap a = unwrap b
  let teq (a : 'a typed) (b : 'a typed) : bool = eq (snd a) (snd b)

  let applyfun (a : t) (b : t list) : t option =
    let a, b = unwrap a, List.map unwrap b in
    match a with
    | Cfun (param, ret) -> if param = b then Some ret else None
    | _ -> None
  ;;
end

module Env = struct
  type entry =
    | Entry of CType.t * int
    | Barrier

  type env =
    { env_raw : (string * entry) list
    ; env_nextid : int
    }

  let empty = { env_raw = []; env_nextid = 0 }

  let add_name name ty env =
    { env_raw = (name, Entry (ty, env.env_nextid)) :: env.env_raw
    ; env_nextid = succ env.env_nextid
    }
  ;;

  let add_scope name env =
    { env_raw = (name, Barrier) :: env.env_raw; env_nextid = env.env_nextid }
  ;;

  let add_pairs pairs env =
    let num = ref env.env_nextid in
    { env_raw =
        List.append
          (List.map
             (fun (a, b) ->
               let id = !num in
               incr num;
               a, Entry (b, id))
             pairs)
          env.env_raw
    ; env_nextid = !num
    }
  ;;

  let get_name (name : string) (env : env) =
    let rec r tl =
      match tl with
      | [] -> None
      | (a1, Entry (ty, _)) :: tl -> if a1 = name then Some ty else r tl
      | _ :: tl -> r tl
    in
    r env.env_raw
  ;;

  let get_name' (name : string) (env : env) =
    let rec r tl =
      match tl with
      | [] -> None
      | (a1, Entry (ty, id)) :: tl -> if a1 = name then Some (ty, id) else r tl
      | _ :: tl -> r tl
    in
    r env.env_raw
  ;;

  let get_current_scope (env : env) =
    let rec r tl =
      match tl with
      | [] -> None
      | (a1, Barrier) :: _ -> Some a1
      | _ :: tl -> r tl
    in
    r env.env_raw
  ;;

  let get_local_name (name : string) (env : env) =
    let rec r tl =
      match tl with
      | (a1, Entry (ty, _)) :: tl -> if a1 = name then Some ty else r tl
      | _ -> None
    in
    r env.env_raw
  ;;

  let get_local_name' (name : string) (env : env) =
    let rec r tl =
      match tl with
      | (a1, Entry (ty, id)) :: tl -> if a1 = name then Some (ty, id) else r tl
      | _ -> None
    in
    r env.env_raw
  ;;

  let is_local_name_type_eq (name : string) (ty : CType.t) (env : env) =
    let rec r tl =
      match tl with
      | (a1, Entry (ty', _)) :: tl ->
        if a1 = name then Some (CType.eq ty ty') else r tl
      | _ -> None
    in
    r env.env_raw
  ;;

  let is_name_type_eq (name : string) (ty : CType.t) (env : env) =
    let rec r tl =
      match tl with
      | [] -> None
      | (a1, Entry (ty', _)) :: tl ->
        if a1 = name then Some (CType.eq ty ty') else r tl
      | _ :: tl -> r tl
    in
    r env.env_raw
  ;;
end
