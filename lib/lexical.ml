module Range = struct
  type pos = Lexing.position

  let pp_pos formater (pos : pos) =
    Format.fprintf
      formater
      "%s:%i:%i"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  ;;

  type t = pos * pos [@@deriving show]
  type 'a ranged = 'a * t

  let to_string ((a, b) : t) =
    let open Printf in
    if a.pos_fname = b.pos_fname
    then
      if a.pos_lnum = b.pos_lnum
      then
        sprintf
          "%s:%i:%i-%i"
          a.pos_fname
          a.pos_lnum
          (a.pos_cnum - a.pos_bol + 1)
          (b.pos_cnum - b.pos_bol + 1)
      else
        sprintf
          "%s:%i:%i-%i:%i"
          a.pos_fname
          a.pos_lnum
          (a.pos_cnum - a.pos_bol + 1)
          b.pos_lnum
          (b.pos_cnum - b.pos_bol + 1)
    else
      sprintf
        "%s:%i:%i-%s:%i:%i"
        a.pos_fname
        a.pos_lnum
        (a.pos_cnum - a.pos_bol + 1)
        b.pos_fname
        b.pos_lnum
        (b.pos_cnum - b.pos_bol + 1)
  ;;

  let join (a : t) (b : t) : t = fst a, snd b
  let dump out t = t |> to_string |> Printf.fprintf out "%s"

  let of_lexbuf (lexbuf : Lexing.lexbuf) =
    lexbuf.lex_start_p, lexbuf.lex_curr_p
  ;;

  let sexp_of_t t = Sexplib0.Sexp.Atom (to_string t)
end

module Error = struct
  exception LexicalError of string * Range.pos
end

module Token = struct
  type pos = Range.pos [@@deriving show]
  type range = Range.t [@@deriving show]

  type token =
    | Tstr (* str *) of string * range
    | Tid (* id *) of string * range
    | Tint (* int *) of int64 * range
    (* 1 *)
    | Tlp (* ( *) of pos
    | Trp (* ) *) of pos
    | Tlb (* [ *) of pos
    | Trb (* ] *) of pos
    | Tlc (* { *) of pos
    | Trc (* } *) of pos
    | Tcomma (* , *) of pos
    | Tbar (* | *) of pos
    | Tquestion (* ? *) of pos
    | Tadd (* + *) of pos
    | Tsub (* - *) of pos
    | Tmul (* * *) of pos
    | Tdiv (* / *) of pos
    | Tsup (* ^ *) of pos
    | Tdot (* . *) of pos
    | Tcolon (* : *) of pos
    | Tsemi (* ; *) of pos
    | Teq (* = *) of pos
    | Tlt (* < *) of pos
    | Tgt (* > *) of pos
    (* 2 *)
    | Tassign (* := *) of pos
    | Tpeq (* == *) of pos
    | Tpneq (* != *) of pos
    | Tneq (* <> *) of pos
    | Tleq (* <= *) of pos
    | Tgeq (* >= *) of pos
    | Tinduce (* => *) of pos
    | Tto (* -> *) of pos
    | Tor (* or *) of pos
    (* 3 *)
    | Tnil (* nil *) of pos
    | Tand (* and *) of pos
    | Tnot (* not *) of pos
    | Txor (* xor *) of pos
    | Tshl (* shl *) of pos
    | Tshr (* shr *) of pos
    (* 4 *)
    | Tlshr (* lshr *) of pos
    | Txnor (* xnor *) of pos
    | Teof
  [@@deriving show]

  let getrng = function
    | Tstr (_, rng) | Tid (_, rng) | Tint (_, rng) -> rng
    (* 1 *)
    | Tlp pos
    | Trp pos
    | Tlb pos
    | Trb pos
    | Tlc pos
    | Trc pos
    | Tcomma pos
    | Tbar pos
    | Tquestion pos
    | Tadd pos
    | Tsub pos
    | Tmul pos
    | Tdiv pos
    | Tsup pos
    | Tdot pos
    | Tcolon pos
    | Tsemi pos
    | Teq pos
    | Tlt pos
    | Tgt pos -> pos, { pos with Lexing.pos_cnum = pos.pos_cnum + 1 }
    (* 2 *)
    | Tassign pos
    | Tpeq pos
    | Tpneq pos
    | Tneq pos
    | Tleq pos
    | Tgeq pos
    | Tinduce pos
    | Tto pos
    | Tor pos -> pos, { pos with Lexing.pos_cnum = pos.pos_cnum + 2 }
    (* 3 *)
    | Tnil pos | Tand pos | Tnot pos | Txor pos | Tshl pos | Tshr pos
      -> pos, { pos with Lexing.pos_cnum = pos.pos_cnum + 3 }
    (* 4 *)
    | Tlshr pos | Txnor pos ->
      pos, { pos with Lexing.pos_cnum = pos.pos_cnum + 4 }
    | Teof -> Lexing.dummy_pos, Lexing.dummy_pos
  ;;
end

module Literal = struct
  exception BadLiteral of string

  type integer =
    { data : int64
    ; sign : integer_sign_type
    ; size : integer_size_type
    }

  and integer_sign_type =
    | Signed
    | Unsigned

  and integer_size_type =
    | Normal
    | Long
    | LongLong

  type floating =
    { data : float
    ; size : float_size_type
    }

  and float_size_type =
    | Float
    | Double
    | LongDouble

  let dumb_int = { data = 0L; sign = Signed; size = Normal }
  let dumb_float = { data = 0.; size = Double }

  let ord ch =
    match ch with
    | '0' -> 0L
    | '1' -> 1L
    | '2' -> 2L
    | '3' -> 3L
    | '4' -> 4L
    | '5' -> 5L
    | '6' -> 6L
    | '7' -> 7L
    | '8' -> 8L
    | '9' -> 9L
    | 'a' | 'A' -> 10L
    | 'b' | 'B' -> 11L
    | 'c' | 'C' -> 12L
    | 'd' | 'D' -> 13L
    | 'e' | 'E' -> 14L
    | 'f' | 'F' -> 15L
    | _ ->
      raise
        (BadLiteral (Printf.sprintf "Not number charactor: %c" ch))
  ;;

  let _int_folder_maker base (acc : integer) ch =
    let adv b =
      match base with
      | 2L -> Int64.shift_left b 1
      | 4L -> Int64.shift_left b 2
      | 8L -> Int64.shift_left b 3
      | 16L -> Int64.shift_left b 4
      | _ -> Int64.mul b base
    in
    match ch with
    | 'l' | 'L' ->
      (match acc.size with
       | Normal -> { acc with size = Long }
       | Long -> { acc with size = LongLong }
       | LongLong -> raise (BadLiteral "Too much suffix"))
    | 'u' | 'U' | 'x' | 'X' | 'b' | 'B' ->
      { acc with sign = Unsigned }
    | '\'' | '+' -> acc
    | '-' -> { acc with data = Int64.neg acc.data }
    | _ as nch ->
      { acc with data = Int64.add (adv acc.data) (ord nch) }
  ;;

  let int_of_dec (data : string) =
    let folder = _int_folder_maker 10L in
    String.fold_left folder dumb_int data
  ;;

  (* Oct starts with 0 *)
  let int_of_oct (data : string) =
    let folder = _int_folder_maker 8L in
    String.fold_left folder dumb_int data
  ;;

  (* Hex starts with 0x or 0X, first 2 char is useless *)
  let int_of_hex (data : string) =
    let folder = _int_folder_maker 16L in
    String.fold_left folder dumb_int data
  ;;

  (* Bin starts with 0b or 0B, first 2 char is useless *)
  let int_of_bin (data : string) =
    let folder = _int_folder_maker 2L in
    String.fold_left folder dumb_int data
  ;;

  let float_of_lit (data : string) =
    let ldata = String.length data - 1 in
    let _data, _size =
      match String.unsafe_get data ldata with
      | 'f' | 'F' -> String.sub data 0 ldata, Float
      | 'l' | 'L' -> String.sub data 0 ldata, LongDouble
      | _ -> data, Double
    in
    { data = float_of_string _data; size = _size }
  ;;

  (* This works surprising well. *)
  let char_of_lit (data : string) =
    Int64.of_int (int_of_char data.[1])
  ;;

  (* useless *)
  (* let string_of_lit (data : string) = String.sub data 1 (String.length data - 2) *)

  (* Fuck ocaml standrad lib *)
  let uint64_to_string (d : int64) =
    let buf = Buffer.create 20 in
    let rec cnv (d : int64) =
      match d with
      | (0L | 1L | 2L | 3L | 4L | 5L | 6L | 7L | 8L | 9L) as t ->
        Buffer.add_char
          buf
          (char_of_int (Int64.to_int t + int_of_char '0'))
      | _ ->
        let dr = Int64.unsigned_rem d 10L in
        let dd = Int64.unsigned_div (Int64.sub d dr) 10L in
        cnv dd;
        cnv dr
    in
    cnv d;
    String.of_bytes @@ Buffer.to_bytes buf
  ;;
end
