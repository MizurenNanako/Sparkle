module Temp = struct
  (* We use the word "temporary" to mean a value that
     is temporarily held in a register and the word
     "label" to mean some machine-language location whose
     exact address is yet to be determined, just like a
     label in assembly language.

     This module manages these two distinct sets of names *)

  (* We represent a "temporary" as a pair of
     integer (identifier) and an optional name,
     which is used for registers (and is helpful for tracing) *)
  type var = int [@@deriving show { with_path = false }]
  type label = int [@@deriving show { with_path = false }]
  type dlabel = int [@@deriving show { with_path = false }]
  type data = dlabel * bytes [@@deriving show { with_path = false }]

  type t =
    { mutable temp_var_next : int
    ; mutable temp_label_next : int
    ; mutable temp_data_next : int
    ; mutable temp_data_list : data list
    }

  let init () =
    { temp_var_next = 0
    ; temp_label_next = 0
    ; temp_data_next = 0
    ; temp_data_list = []
    }
  ;;

  let new_var tempmgr =
    let r = tempmgr.temp_var_next in
    tempmgr.temp_var_next <- succ tempmgr.temp_var_next;
    r
  ;;

  let new_label tempmgr =
    let r = tempmgr.temp_label_next in
    tempmgr.temp_label_next <- succ tempmgr.temp_label_next;
    r
  ;;

  let new_data (data : bytes) tempmgr =
    let r = tempmgr.temp_data_next in
    tempmgr.temp_data_next <- succ tempmgr.temp_data_next;
    tempmgr.temp_data_list <- (r, data) :: tempmgr.temp_data_list;
    r
  ;;
end

(* 2 types of basic instructs *)
module IR = struct
  type expr =
    (* basic expr, all from two operand *)
    | Add of expr * expr
    | And of expr * expr
    | Div of expr * expr
    | Mul of expr * expr
    | Nor of expr * expr
    | Or of expr * expr
    | Sll of expr * expr
    | Sra of expr * expr
    | Srl of expr * expr
    | Sub of expr * expr
    | Xor of expr * expr
    | Slt of expr * expr
    | Sltu of expr * expr
    | Sge of expr * expr
    | Sgeu of expr * expr
    | Sgt of expr * expr
    | Sgtu of expr * expr
    | Sle of expr * expr
    | Sleu of expr * expr
    | Seq of expr * expr
    | Sne of expr * expr
    (* from memory location, with offset *)
    | EMem of expr * expr
    (* from return value of call, first expr will evaluate to label *)
    | ECall of expr * expr list
    (* same, but tail call *)
    | ETCall of expr * expr list
    (* from 2 branches, if first expr > 0, the second, else the third *)
    | EBr of expr * expr * expr
    (* from a value less statement then expr *)
    | Eseq of stmt * expr
    (* from immediate but a label *)
    | ELabel of Temp.label
    (* from immediate *)
    | EIm of int64
    (* from .data *)
    | EData of Temp.dlabel
    (* from reg/mem *)
    | EVar of Temp.var

  and stmt =
    (* call without return value *)
    | Call of expr * expr list
    (* call without return value, but tail *)
    | TCall of expr * expr list
    (* unconditional jump *)
    | Jump of Temp.label
    (* from 2 branches, if expr > 0, the first stmt, else the second *)
    | Br of expr * stmt * stmt
    (* chained *)
    | Seq of stmt * stmt
    (* low level label *)
    | Label of Temp.label
end
