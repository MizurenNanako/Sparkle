module CType = struct
  type t =
    | Cint
    | Clist
    | Cbytes
    | Cunit
    | Cfun of t list * t
    | Cdecl of t
  [@@deriving show { with_path = false }]

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
    | "str" | "string" | "bytes" | "ptr" | "b" -> Some Cbytes
    | _ -> None
  ;;

  let eq (a : t) (b : t) : bool = unwrap a = unwrap b
  let neq (a : t) (b : t) : bool = unwrap a <> unwrap b
  let teq (a : 'a typed) (b : 'a typed) : bool = eq (snd a) (snd b)

  let applyfun (a : t) (b : t list) : t option =
    let a, b = unwrap a, List.map unwrap b in
    match a with
    | Cfun (param, ret) -> if param = b then Some ret else None
    | _ -> None
  ;;
end

module Env = struct
  type id =
    | IdTmp of int
    | IdCst of int
    | IdArg of int
  [@@deriving show { with_path = false }]

  (* --- some dirty workaround here!!! --- *)
  type dirty_state =
    { mutable dirty_next_id : int
    ; mutable dirty_next_cid : int
    }

  type dirty_table = { mutable dirty_constant_table : (id * bytes) list }
  [@@deriving show { with_path = false }]

  let dirty_table = { dirty_constant_table = [] }
  let dirty_state = { dirty_next_id = -1; dirty_next_cid = -1 }

  let _nextid () =
    dirty_state.dirty_next_id <- succ dirty_state.dirty_next_id;
    dirty_state.dirty_next_id
  ;;

  let _nextcid () =
    dirty_state.dirty_next_cid <- succ dirty_state.dirty_next_cid;
    dirty_state.dirty_next_cid
  ;;

  let _dirty_str id str =
    dirty_table.dirty_constant_table
    <- (id, str) :: dirty_table.dirty_constant_table
  ;;

  let _resetid () = dirty_state.dirty_next_id <- -1
  let _resetcid () = dirty_state.dirty_next_cid <- -1
  let _get_dirty_state () = dirty_state.dirty_next_id, dirty_state.dirty_next_cid

  let _set_dirty_state (next_id, next_cid) =
    (match next_id with
     | Some next_id -> dirty_state.dirty_next_id <- next_id
     | None -> ());
    match next_cid with
    | Some next_cid -> dirty_state.dirty_next_cid <- next_cid
    | None -> ()
  ;;

  (* ------------------------------------- *)

  type entry =
    | Entry of CType.t * id
    | Barrier
  [@@deriving show { with_path = false }]

  type env =
    { env_raw : (string * entry) list
    (* ; env_nextid : int *)
    (* ; env_nextcid : int *)
    }
  [@@deriving show { with_path = false }]

  let empty = { env_raw = [] }
  (* let empty = { env_raw = []; env_nextid = 0; env_nextcid = 0 } *)

  let add_name' name ty env =
    (* let id = env.env_nextid in *)
    let id = _nextid () in
    ( { env_raw =
          (name, Entry (ty, IdTmp id)) :: env.env_raw
          (* ; env_nextid = succ env.env_nextid *)
      }
    , IdTmp id )
  ;;

  let add_name name ty env = add_name' name ty env |> fst

  let add_tmp' ty env =
    (* let id = env.env_nextid in *)
    let id = _nextid () in
    ( { (* env with *)
        env_raw =
          (string_of_int id, Entry (ty, IdTmp id)) :: env.env_raw
          (* ; env_nextid = succ env.env_nextid *)
      }
    , IdTmp id )
  ;;

  let add_tmp ty env = add_tmp' ty env |> fst

  let add_cname' name ty env =
    (* let id = env.env_nextcid in *)
    let id = _nextcid () in
    ( { (* env with *)
        env_raw =
          (name, Entry (ty, IdCst id)) :: env.env_raw
          (* ; env_nextcid = succ env.env_nextcid *)
      }
    , IdCst id )
  ;;

  let add_cname name ty env = add_cname' name ty env |> fst
  let add_scope name env = { env_raw = (name, Barrier) :: env.env_raw }

  let add_args (pairs : (string * CType.t) list) (env : env) =
    let argnext = ref 0 in
    let ids = ref [] in
    let pairs =
      List.map
        (fun (s, t) ->
          let the_arg = !argnext in
          ids := IdArg the_arg :: !ids;
          incr argnext;
          s, Entry (t, IdArg the_arg))
        pairs
    in
    { env_raw = List.append pairs env.env_raw }, !ids
  ;;

  (* { env with env_raw = List.append pairs env.env_raw }, !argnext *)

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

  let impl_cname' name ty env =
    match get_local_name' name env with
    | None ->
      (* let id = env.env_nextcid in *)
      let id = _nextcid () in
      ( { (* env with *)
          env_raw =
            (name, Entry (ty, IdCst id)) :: env.env_raw
            (* ; env_nextcid = succ env.env_nextcid *)
        }
      , IdCst id )
    | Some (_, id) -> { env_raw = (name, Entry (ty, id)) :: env.env_raw }, id
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

  let is_local_name_type_eq' (name : string) (ty : CType.t) (env : env) =
    let rec r tl =
      match tl with
      | (a1, Entry (ty', _)) :: tl ->
        if a1 = name then Some (CType.eq ty ty', ty') else r tl
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

  (* get recent one sig of name, destory it, set next to its id *)
  (* let hang_up (name : string) (env : env) : env * int =
    let rec r acc tl =
      match tl with
      | [] -> raise @@ Failure "hang_up"
      | ((a1, Entry (CType.Cdecl _, IdCst id)) as the) :: tl ->
        if a1 = name then List.rev_append acc tl, id else r (the :: acc) tl
      | (_ as the) :: _ -> r (the :: acc) tl
    in
    let fixed, id = r [] env.env_raw in
    { env with env_raw = fixed; env_nextcid = id }, env.env_nextcid
  ;;

  let hang_down (cid : int) (env : env) : env = { env with env_nextcid = cid } *)
end
