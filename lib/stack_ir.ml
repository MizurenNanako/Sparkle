module StkIR = struct
  type instruction =
    | Push
    | Pop
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
    | Not
    | And
    | Xor
    | Or
    | Sllv
    | Srlv
    | Srav
    | Sll of int
    | Srl of int
    | Sra of int
    | Immediate of int
    | Label of string
    | LoadConst of string (* const word from .data *)
    | LoadAddr of int (* pop addr, offset *)
    | StoreAddr of int (* pop addr, pop 1 word out and store, offset *)

  let op1 name = [ "pop $t1"; name ^ " $t1, $t1"; "push $1" ]

  let op1i name imm =
    [ "pop $t1"; Printf.sprintf "%s $t1, $t1, %i" name imm; "push $1" ]
  ;;

  let op2 name = [ "pop $t1"; "pop $t2"; name ^ " $t1, $t1, $t2"; "push $1" ]

  let to_asm =
    let open Printf in
    function
    | Push -> [ "push $t1" ]
    | Pop -> [ "pop $t1" ]
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
    | Immediate imm -> [ "li $t1, " ^ string_of_int imm ]
    | Label lab -> [ "la $t1, " ^ lab ]
    | LoadConst name -> [ "lw $t1, " ^ name ]
    | LoadAddr offset -> [ "pop $t2"; sprintf "lw $1, %i($2)" offset; "push $1" ]
    | StoreAddr offset -> [ "pop $t2"; "pop $t1"; sprintf "sw $t1 %i($2)" offset ]
    | _ -> [ "trap" ]
  ;;
end
