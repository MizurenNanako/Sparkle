module StkIR = struct
  exception NotImplanted

  type instruction =
    (* basic push pop *)
    | Push
    | Pop
    | Dup (* duplicate top *)
    (* pop, pop, op, push *)
    | Add
    | Addu
    | Sub
    | Subu
    | Mul
    | Mulu
    | Div
    | Divu
    | Rem
    | Remu
    | And
    | Xor
    | Or
    | Sllv
    | Srlv
    | Srav
    (* pop, op, push *)
    | Not
    | Sll of int
    | Srl of int
    | Sra of int
    (* push(data) *)
    | LoadImme of int
    | LoadLabel of string
    | LoadConst of string (* const word from .data *)
    (* pop addr, load addr, push *)
    | LoadAddr of int (* pop addr, offset *)
    (* pop addr, pop data, store data at addr *)
    | StoreAddr of int (* pop addr, pop 1 word out and store, offset *)
    (* jump *)
    | JumpLabel of string (* jump to label *)
    (* call to label, assuming all parameters
       pushed into stack in definition order,
       for their id's order are already reversed. *)
    | PreCall (* this is to allocate return value (4 bytes) *)
    (* | PreCall2 this is to allocate return value (8 bytes) *)
    (* after pre, push args, then call *)
    (* | PreArg *)
    | Call of string
    (* tail recursive, copy args, rewind stack. *)
    (* | TailRec *)
    (* return *)
    | Ret of int (* need argscnt, $sp = ret pos *)
    | JumpAddr (* pop addr, jump to addr *)
    (* branch to label *)
    | Beq of string
    | Bne of string
    | Blt of string
    | Ble of string
    | Bgt of string
    | Bge of string
    | Bltu of string
    | Bleu of string
    | Bgtu of string
    | Bgeu of string
    (* acturally useable *)
    | Blez of string
    | Bgtz of string
    (* syscall *)
    | SysCall of syscall_mode
    (* load func args *)
    | LoadArg of int
    | LoadTmp of int
    (* store return value *)
    | StoreRet of int
  (* need offset *)
  (* | StoreRet2 *)

  and syscall_mode =
    | SysPrintInt (* data *)
    (* | SysPrintFloat *)
    (* | SysPrintDouble *)
    | SysPrintString (* addr *)
    | SysReadInt
    (* | SysReadFloat *)
    (* | SysReadDouble *)
    | SysReadString (* addr, length *)
    | SysAlloc (* size *)
    | SysExit
    | SysPrintChar (* char *)
    | SysReadChar

  type manager =
    { manager_next_label_id : int
    ; manager_next_data_id : int
    }

  (*
     frame:
     fp+n | return value
     ...
     fp+8 | arg1
     fp+4 | arg0
     fp   | --------frame---------
     fp-4 | old fp
     fp-8 | old ra
     fp-12| tmp1
  *)

  type frame =
    { frame_id : int
    ; frame_args_cnt : int (* return value is at fp+(args_cnt * 4)+4 *)
    ; frame_tmp_cnt : int (* allocate 4*tmp_cnt *)
    ; frame_content : instruction list
    }

  let syscall_num = function
    | SysPrintInt -> "1"
    (* | SysPrintFloat -> "2" *)
    (* | SysPrintDouble -> "3" *)
    | SysPrintString -> "4"
    | SysReadInt -> "5"
    (* | SysReadFloat -> "6" *)
    (* | SysReadDouble -> "7" *)
    | SysReadString -> "8"
    | SysAlloc -> "9"
    | SysExit -> "10"
    | SysPrintChar -> "11"
    | SysReadChar -> "12"
  ;;

  let op1i name imm =
    [ "pop $t1"; Printf.sprintf "%s $t1, $t1, %i" name imm; "push $1" ]
  ;;

  let op1 name = [ "pop $t1"; name ^ " $t1, $t1"; "push $1" ]
  let op2 name = [ "pop $t1"; "pop $t2"; name ^ " $t1, $t1, $t2"; "push $1" ]
  let opbr name label = [ "pop $t1"; "pop $t2"; name ^ " $t1, $t2, " ^ label ]
  let syscall mode = [ "li $v0, " ^ syscall_num mode; "syscall" ]

  let ins_to_asm =
    let open Printf in
    function
    | Push -> [ "push $t1" ]
    | Pop -> [ "pop $t1" ]
    | Dup -> [ "lw $t1, 0($sp)"; "pop $1" ]
    | Add -> op2 "add"
    | Addu -> op2 "addu"
    | Sub -> op2 "sub"
    | Subu -> op2 "subu"
    | Mul -> op2 "mul"
    | Mulu -> op2 "mulu"
    | Div -> op2 "div"
    | Divu -> op2 "divu"
    | Rem -> op2 "rem"
    | Remu -> op2 "remu"
    | Not -> op2 "not"
    | And -> op2 "and"
    | Xor -> op2 "xor"
    | Or -> op1 "or"
    | Sllv -> op2 "sllv"
    | Srlv -> op2 "srlv"
    | Srav -> op2 "srav"
    | Sll imm -> op1i "sll" imm
    | Srl imm -> op1i "srl" imm
    | Sra imm -> op1i "sra" imm
    | LoadImme imm -> [ "li $t1, " ^ string_of_int imm; "push $t1" ]
    | LoadLabel lab -> [ "la $t1, " ^ lab; "push $t1" ]
    | LoadConst name -> [ "lw $t1, " ^ name; "push $t1" ]
    | LoadAddr offset -> [ "pop $t2"; sprintf "lw $1, %i($2)" offset; "push $t1" ]
    | LoadArg i -> [ sprintf "lw $t1, %i($fp)" ((i * 4) + 4); "push $t1" ]
    | LoadTmp i -> [ sprintf "lw $t1, %i($fp)" ((-i * 4) - 8); "push $t1" ]
    | StoreAddr offset -> [ "pop $t2"; "pop $t1"; sprintf "sw $t1 %i($2)" offset ]
    | JumpLabel label -> [ "j " ^ label ]
    | PreCall -> [ "addi $sp, $sp, -4"; "move $a3 $sp" ]
    | Call label -> [ "jal " ^ label ]
    | StoreRet argscnt ->
      [ "pop $t1"; sprintf "sw $t1, %i($fp)" ((argscnt * 4) + 4) ]
    | Ret argscnt ->
      [ sprintf "addi $sp, $fp, %i" ((argscnt * 4) + 4)
      ; "lw $ra -8($fp)"
      ; "lw $fp -4($fp)"
      ; "jr $ra"
      ]
    | JumpAddr -> [ "pop $t1"; "jr $t1" ]
    | Beq label -> opbr "beq" label
    | Bne label -> opbr "bne" label
    | Blt label -> opbr "blt" label
    | Ble label -> opbr "ble" label
    | Blez label -> [ "pop $t1"; "blez $t1, " ^ label ]
    | Bgtz label -> [ "pop $t1"; "bgtz $t1, " ^ label ]
    | Bgt label -> opbr "bgt" label
    | Bge label -> opbr "bge" label
    | Bltu label -> opbr "bltu" label
    | Bleu label -> opbr "bleu" label
    | Bgtu label -> opbr "bgtu" label
    | Bgeu label -> opbr "bgeu" label
    | SysCall (SysPrintInt as m) -> [ "pop $a0" ] @ syscall m
    | SysCall (SysPrintString as m) -> [ "pop $a0" ] @ syscall m
    | SysCall (SysReadInt as m) -> syscall m @ [ "push $v0" ]
    | SysCall (SysReadString as m) -> [ "pop $a0"; "pop $a1" ] @ syscall m
    | SysCall (SysAlloc as m) -> [ "pop $a0" ] @ syscall m @ [ "push $v0" ]
    | SysCall (SysExit as m) -> syscall m
    | SysCall (SysPrintChar as m) -> [ "pop $a0" ] @ syscall m
    | SysCall (SysReadChar as m) -> syscall m @ [ "push $v0" ]
  ;;

  (* | _ -> raise NotImplanted *)

  (* remove some stupid part *)
  let deduplicate (asm : string list) =
    let rec r acc tl =
      match tl with
      | [] -> List.rev acc
      | "push $t1" :: "pop $t1" :: tl -> r acc tl
      | hd :: tl -> r (hd :: acc) tl
    in
    r [] asm
  ;;
end
