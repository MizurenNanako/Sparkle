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

  type t =
    { mutable temp_var_next : int
    ; mutable temp_label_next : int
    }

  let init () = { temp_var_next = 0; temp_label_next = 0 }

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
end

module IR = struct
  (** Represents a computation of some value (possibly with side effects) *)
  type expr =
    (* Integer constant *)
    | Const of int
    (* Symbolic constant corresponding to
       an assembly language label *)
    | Name of Temp.label
    (* Temporary in abstract machine is similar to a register in a real machine.
       However, the abstract machine has an infinite number of temporaries *)
    | Temp of Temp.var
    (* Application of binary operator [op] to operands of type [expr].
       The left subexpression is evaluated before the right subexpression *)
    | Arith of expr * arithop * expr
    (* Represents contents of [Frame.word_size] bytes of memory starting at
       address [expr]. If [Mem] is used as the left child of
       a [Move] it means "store", but anywhere else it means "fetch" *)
    | Mem of expr
    (* Procedure call: the application of a function [f] to
       argument list [l]. The subexpression [f] is evaluated before the
       arguments which are evaluated from left to right *)
    | Call of expr * expr list
    (* The statement [stmt] is evaluated for side effects,
       then [expr] is evaluated for a result *)
    | ESeq of stmt * expr
  [@@deriving show { with_path = false }]

  (* Statements of the IR language perform side effects and control flow *)
  and stmt =
    (* There are 2 options:
       - Move (Temp.t, e) - Evaluate [e] and move it into temporary
       - Move (Mem(e1), e2) - Evaluate e1, yielding address [a].
         Then evaluate [e2], and store the result into
         [Frame.word_size] bytes of memory starting at [a] *)
    | Move of expr * expr
    (* Evaluate [expr] and discard the result *)
    | Expr of expr
    (* Transfer control (jump) to address [expr].
       The destination [expr] may be a literal label, as in [Name lab],
       or it may be an address calculated by any other kind of expression.
       The list of labels [Temp.label list] specifies all the
       possible locations that the expression [e] can evaluate to
       (this is necessary for dataflow analysis later) *)
    | Jump of expr * Temp.label list
    (* Evaluate [left], [right] in that order, yielding values [l], [r].
       Then compare [l], [r] using the relational operator [op].
       If the result is [true], jump to [t]; otherwise jump to [f] *)
    | CJump of
        { op : relop
        ; left : expr
        ; right : expr
        ; target : Temp.label
        }
    (* Two consequent statements *)
    | Seq of stmt * stmt
    (* Define the constant value of name [n] to be the current machine code address.
       This is like a label definition in assembly language. The value
       [Label n] may be the target of jumps, calls, etc *)
    | Label of Temp.label
  [@@deriving show { with_path = false }]

  (* The integer arithmetic operators are [Plus], [Minus], [Mul] and [Div].
     Integer bitwise logical operators are [And], [Or] and [Xor].
     Integer local shift operators [LShift] and [RShift].
     Integer arithmetic right-shift is [ARShift] *)
  and arithop =
    | Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Xor
    | Xnor
    | Shl
    | Shr
    | Lshr
  [@@deriving show { with_path = false }]

  (* The relational operators are [Eq] and [Ne] for
     integer equality and nonequality (signed or unsigned).
     Signed integer inequalities [Lt], [Gt], [Le] and [Ge].
     Unsigned integer inequalities [Ult], [Ule], [Ugt] and [Uge] *)
  and relop =
    | Eq
    | Ne
    | Lt
    | Gt
    | Le
    | Ge
    | Ult
    | Ule
    | Ugt
    | Uge
  [@@deriving show { with_path = false }]

  (* Makes a [arithop] out of a [Syntax.op] *)
  let binop_of_op =
    let open Syntactics.AST in
    function
    | OpAdd -> Add
    | OpSub -> Sub
    | OpMul -> Mul
    | OpDiv -> Div
    | OpAnd -> And
    | OpOr -> Or
    | OpXor -> Xor
    | OpXnor -> Xnor
    | OpShl -> Shl
    | OpShr -> Shr
    | OpLshr -> Lshr
  ;;

  (* Makes a [relop] out of a [Syntax.op] *)
  let relop_of_op =
    let open Syntactics.AST in
    function
    | OpGeq -> Ge
    | OpGt -> Gt
    | OpLeq -> Le
    | OpLt -> Lt
    | OpEq -> Eq
    | OpNeq -> Ne
    | op -> failwith @@ "Unsupported relational operator: " ^ show_relop op
  ;;

  (* Inverses given relational operator *)
  let not_rel = function
    | Eq -> Ne
    | Ne -> Eq
    | Lt -> Ge
    | Ge -> Lt
    | Gt -> Le
    | Le -> Gt
    | Ult -> Uge
    | Uge -> Ult
    | Ule -> Ugt
    | Ugt -> Ule
  ;;
end
