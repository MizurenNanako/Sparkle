type label = string

type reg =
  | R0
  | R1 (* DO NOT USE ME:  assembler temp *)
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | R16
  | R17
  | R18
  | R19
  | R20
  | R21
  | R22
  | R23
  | R24
  | R25
  | R26
  | R27 (* DO NOT USE ME: reserved for OS *)
  | R28
  | R29
  | R30
  | R31 (* used for special purposes... *)

type operand =
  | Reg of reg
  | Immed of Word32.word

type inst =
  | Push of reg
  | Add of reg * reg * operand
  | And of reg * reg * operand
  | Div of reg * reg * reg
  | Mul of reg * reg * reg
  | Nor of reg * reg * reg
  | Or of reg * reg * operand
  | Sll of reg * reg * operand
  | Sra of reg * reg * operand
  | Srl of reg * reg * operand
  | Sub of reg * reg * reg
  | Xor of reg * reg * operand
  | Li of reg * Word32.word
  | Slt of reg * reg * operand
  | Sltu of reg * reg * operand
  | Sge of reg * reg * reg
  | Sgeu of reg * reg * reg
  | Sgt of reg * reg * reg
  | Sgtu of reg * reg * reg
  | Sle of reg * reg * reg
  | Sleu of reg * reg * reg
  | Seq of reg * reg * reg
  | Sne of reg * reg * reg
  | B of label
  | Beq of reg * reg * label
  | Bgez of reg * label
  | Bgtz of reg * label
  | Blez of reg * label
  | Bltz of reg * label
  | Bne of reg * reg * label
  | J of label
  | Jal of label
  | Jr of reg
  | Jalr of reg * reg
  | La of reg * label
  | Lb of reg * reg * Word32.word
  | Lbu of reg * reg * Word32.word
  | Lh of reg * reg * Word32.word
  | Lhu of reg * reg * Word32.word
  | Lw of reg * reg * Word32.word
  | Sb of reg * reg * Word32.word
  | Sh of reg * reg * Word32.word
  | Sw of reg * reg * Word32.word
  | Label of label

(* The functions below are used to convert MIPS instructions
 * into strings, suitable for output to an assembly file. *)
let reg2string (r : reg) : string =
  match r with
  | R0 -> "$zero"
  | R1 -> "$at"
  | R2 -> "$v0"
  | R3 -> "$v1"
  | R4 -> "$a0"
  | R5 -> "$a1"
  | R6 -> "$a2"
  | R7 -> "$a3"
  | R8 -> "$t0"
  | R9 -> "$t1"
  | R10 -> "$t2"
  | R11 -> "$t3"
  | R12 -> "$t4"
  | R13 -> "$t5"
  | R14 -> "$t6"
  | R15 -> "$t7"
  | R16 -> "$s0"
  | R17 -> "$s1"
  | R18 -> "$s2"
  | R19 -> "$s3"
  | R20 -> "$s4"
  | R21 -> "$s5"
  | R22 -> "$s6"
  | R23 -> "$s7"
  | R24 -> "$t8"
  | R25 -> "$t9"
  | R26 -> "$k0"
  | R27 -> "$k1"
  | R28 -> "$gp"
  | R29 -> "$sp"
  | R30 -> "$fp"
  | R31 -> "$ra"
;;

type fmt =
  | R of reg
  | L of label
  | W of Word32.word

let fmt2string (f : fmt) : string =
  match f with
  | R r -> reg2string r
  | W w -> Word32.toStringX w
  | L x -> x
;;

let i2s (i : string) (x : fmt list) : string =
  "\t" ^ i ^ "\t" ^ String.concat ", " (List.map fmt2string x)
;;

let w2s (w : Word32.word) : string =
  if Word32.andb (w, 0x80000000l) = 0x80000000l
  then (
    let wneg = Word32.neg w in
    "-" ^ Word32.toString wneg)
  else Word32.toString w
;;

let i2as (i : string) ((r1 : reg), (r2 : reg), (w : Word32.word)) : string =
  "\t" ^ i ^ "\t" ^ reg2string r1 ^ ", " ^ w2s w ^ "(" ^ reg2string r2 ^ ")"
;;

(* Converts an instruction to a string *)
let inst2string (i : inst) : string =
  match i with
  | Push r1 -> i2s "push" [ R r1 ]
  | Add (r1, r2, Reg r3) -> i2s "add" [ R r1; R r2; R r3 ]
  | Add (r1, r2, Immed w) -> i2s "addi" [ R r1; R r2; W w ]
  | And (r1, r2, Reg r3) -> i2s "and" [ R r1; R r2; R r3 ]
  | And (r1, r2, Immed w) -> i2s "andi" [ R r1; R r2; W w ]
  | Div (r1, r2, r3) -> i2s "div" [ R r1; R r2; R r3 ]
  | Mul (r1, r2, r3) -> i2s "mul" [ R r1; R r2; R r3 ]
  | Nor (r1, r2, r3) -> i2s "nor" [ R r1; R r2; R r3 ]
  | Or (r1, r2, Reg r3) -> i2s "or" [ R r1; R r2; R r3 ]
  | Or (r1, r2, Immed w) -> i2s "ori" [ R r1; R r2; W w ]
  | Sll (r1, r2, Reg r3) -> i2s "sllv" [ R r1; R r2; R r3 ]
  | Sll (r1, r2, Immed w) -> i2s "sll" [ R r1; R r2; W w ]
  | Sra (r1, r2, Reg r3) -> i2s "srav" [ R r1; R r2; R r3 ]
  | Sra (r1, r2, Immed w) -> i2s "sra" [ R r1; R r2; W w ]
  | Srl (r1, r2, Reg r3) -> i2s "srlv" [ R r1; R r2; R r3 ]
  | Srl (r1, r2, Immed w) -> i2s "srl" [ R r1; R r2; W w ]
  | Sub (r1, r2, r3) -> i2s "sub" [ R r1; R r2; R r3 ]
  | Xor (r1, r2, Reg r3) -> i2s "xor" [ R r1; R r2; R r3 ]
  | Xor (r1, r2, Immed w) -> i2s "xori" [ R r1; R r2; W w ]
  | Li (r, w) -> i2s "li" [ R r; W w ]
  | Slt (r1, r2, Reg r3) -> i2s "slt" [ R r1; R r2; R r3 ]
  | Slt (r1, r2, Immed w) -> i2s "slti" [ R r1; R r2; W w ]
  | Sltu (r1, r2, Reg r3) -> i2s "sltu" [ R r1; R r2; R r3 ]
  | Sltu (r1, r2, Immed w) -> i2s "sltiu" [ R r1; R r2; W w ]
  | Sge (r1, r2, r3) -> i2s "sge" [ R r1; R r2; R r3 ]
  | Sgeu (r1, r2, r3) -> i2s "sgeu" [ R r1; R r2; R r3 ]
  | Sgt (r1, r2, r3) -> i2s "sgt" [ R r1; R r2; R r3 ]
  | Sgtu (r1, r2, r3) -> i2s "sgtu" [ R r1; R r2; R r3 ]
  | Sle (r1, r2, r3) -> i2s "sle" [ R r1; R r2; R r3 ]
  | Sleu (r1, r2, r3) -> i2s "sleu" [ R r1; R r2; R r3 ]
  | Sne (r1, r2, r3) -> i2s "sne" [ R r1; R r2; R r3 ]
  | Seq (r1, r2, r3) -> i2s "seq" [ R r1; R r2; R r3 ]
  | B x -> "\tb " ^ x
  | Beq (r1, r2, x) -> i2s "beq" [ R r1; R r2; L x ]
  | Bgez (r1, x) -> i2s "bgez" [ R r1; L x ]
  | Bgtz (r1, x) -> i2s "bgtz" [ R r1; L x ]
  | Blez (r1, x) -> i2s "blez" [ R r1; L x ]
  | Bltz (r1, x) -> i2s "bltz" [ R r1; L x ]
  | Bne (r1, r2, x) -> i2s "bne" [ R r1; R r2; L x ]
  | J x -> "\tj " ^ x
  | Jal x -> "\tjal " ^ x
  | Jr r1 -> i2s "jr" [ R r1 ]
  | Jalr (r1, r2) -> i2s "jalr" [ R r1; R r2 ]
  | La (r1, x) -> i2s "la" [ R r1; L x ]
  | Lb (r1, r2, w) -> i2as "lb" (r1, r2, w)
  | Lbu (r1, r2, w) -> i2as "lbu" (r1, r2, w)
  | Lh (r1, r2, w) -> i2as "lh" (r1, r2, w)
  | Lhu (r1, r2, w) -> i2as "lhu" (r1, r2, w)
  | Lw (r1, r2, w) -> i2as "lw" (r1, r2, w)
  | Sb (r1, r2, w) -> i2as "sb" (r1, r2, w)
  | Sh (r1, r2, w) -> i2as "sh" (r1, r2, w)
  | Sw (r1, r2, w) -> i2as "sw" (r1, r2, w)
  | Label x -> x ^ ":"
;;
