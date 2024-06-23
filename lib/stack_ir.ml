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
    | PreCall1 (* this is to allocate return value (4 bytes) *)
    | PreCall2 (* this is to allocate return value (8 bytes) *)
    (* after pre, push args, then call *)
    (* | PreArg *)
    | Call of string
    | PreTCall (* get to the first arg, set its addr to $t8 *)
    | TArg (* push but respect $t8 *)
    | TCall of string (* tail call *)
    (* return *)
    | Ret
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
    (* syscall *)
    | SysCall of syscall_mode
    (* load func args *)
    | LoadArg of int
    (* store return value *)
    | StoreRet1
    | StoreRet2

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
    | StoreAddr offset -> [ "pop $t2"; "pop $t1"; sprintf "sw $t1 %i($2)" offset ]
    | JumpLabel label -> [ "j " ^ label ]
    | PreCall1 -> [ "addi $sp, $sp, -4"; "move $a3 $sp" ]
    | PreCall2 -> [ "addi $sp, $sp, -8"; "move $a3 $sp" ]
    | Call label -> [ "push $ra"; "push $fp"; "addi $fp, $sp, 8"; "jal " ^ label ]
    | PreTCall -> [ "move $t8, a3" ]
    | TArg -> [ "pop t1"; "sw $t1, $t8"; "addi $t8, $t8, -4" ]
    | TCall label ->
      [ "lw $ra, -4($fp)" (* retrive old $ra *)
      ; "move $sp, $t8" (* restore $sp right at last arg *)
      ; "push $ra" (* same as call *)
      ; "push $fp" (* same as call *)
      ; "addi $fp, $sp, 8" (* same as call *)
      ; "jal " ^ label (* same as call *)
      ]
    | StoreRet1 -> [ "pop $t1"; "sw $t1, 4($fp)" ]
    | StoreRet2 -> [ "pop $t1"; "sw $t1, 0($fp)" ]
    | Ret -> [ "lw $ra, -4($fp)"; "move $sp, $fp"; "lw $fp -8($fp)"; "jr $ra" ]
    | LoadArg i -> [ sprintf "lw $t1, %i($fp)" (i * 4); "push $t1" ]
    | JumpAddr -> [ "pop $t1"; "jr $t1" ]
    | Beq label -> opbr "beq" label
    | Bne label -> opbr "bne" label
    | Blt label -> opbr "blt" label
    | Ble label -> opbr "ble" label
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
