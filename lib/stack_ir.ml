module StkIR = struct
  exception NotImplanted

  (* note all instructions are for system word *)
  (* int32 on mips32 *)

  type word = int32

  let size_of_word = 4

  (* !!! ALL ADDR is not physical address, ther are phy_addr/sizeof(word) !!! *)

  (** the stkrec_addr is calculated with relative address,
      for (addr=n, size=1), it means offset($fp),
      where offset = (size_of_word * size)
      note that args are outside of frame!!! *)
  type stack_record =
    { stkrec_addr : int (* after a push, then set, so starts at 1 *)
    ; stkrec_size : int
    }

  (* note that for a new frame, stack_addr is 0, but the first val will always be 1 *)

  let get_tmp_offset (stkrec : stack_record) = -stkrec.stkrec_addr * size_of_word

  type segdata_record =
    { segd_symbol : string
    ; segd_raw : bytes
    }

  (* all can be access with [List.assoc] *)
  type lookup_table = (string * stack_record) list ref
  type data_table = (string * segdata_record) list ref

  (* some instruction will cause stack growth or shrink,
     which will recorded in [activity_record].
     local var have names and addr in [lookup_table]
     arguments will be in [args_table] of a [frame]
  *)

  (*
     consider [name a + name b]
     1. LOAD_NAME b
     2. LOAD_NAME a
     3. OP2 ADD
  *)

  (*
     consider [name a <- expr e]
     1. compute e
     / now tos is its value
     2. STORE_NAME a
  *)

  (*
     consider [let a = b in c]
     / don't alloc for a, just store (a, addr) to lookup table
     1. BIND a
     2. do the work of b
     / now the top of stack is [value_from_b; ...], acturally a
     3. do the work of c
     4. POP
  *)

  (*
     consider [if p then a else b]
     1. calc p
     / now tos is p
     2. JZ label_else
     / if tos = 0, jump to label_else
     3. do the work of a
     4. J label_end
     5. LABEL label_else
     6. do the work of b
     7. LABEL label_end
  *)

  (*
     consider [call f a1, a2, ..., an]
     / alloc for return val
     1. ALLOC_RET frame_retval_size
     / eval an, ..., a2, a1
     2. do the work for an ... a1
     3. CALL f
     4. POPN sum(frame_params_sizes)

     so in this model, functions do not clean by itself.
  *)

  (*
     consider a function body with frame_record
     / store frame pointer
     1. FRAME_ENTER
     2. do the job
     3. FRAME_EXIT
  *)

  type frame_record =
    { frame_params_sizes : int list (** size of arg1, arg2, ..., argn *)
    ; frame_retval_size : int (** size of return value *)
    ; frame_code : instruction list
    }

  and frame_table = (string * frame_record) list ref

  and instruction =
    | NOP (* do nothing *)
    | IM of word (* pushin an int value *)
    | POP (* remove top of stack *)
    | POPN of int (* pop n times *)
    | OP2 of op2 (* a<-pop, b<-pop, push<-(a op b) *)
    | OP1 of op1
    | BIND of string * int
      (* store name to lookup table without alloc,
         this uses unalloced space, but will be filled
         - after some exec. *)
    | ALLOC of string * int
    (* alloc for a name, the name and addr is then stored in [local_sym_table] *)
    | STORE_NAME of
        string (* store tos to the name, get size info from lookup table *)
    | LOAD_NAME of string (* load name to tos *)
    | LOAD_CONST of string (* load from .data to tos *)
    | ALLOC_RET of int (* alloc n words for return value *)
    | STORE_RET
      (* store tos, tos+1, ..., to return value, respecting [frame_retval_size] *)
    | LABEL of string (* mark a label, for later jumping *)
    | J of string (* jump to label *)
    | JZ of string (* if tos = 0, jump to label *)
    | RET (* just return with nothing *)
    | CALL of string (* call to function with label from [frame_table] *)
    | LOAD_ARG of
        int (* load the nth arg (start from 0), respecting [frame_params_sizes] *)
    | FRAME_ENTER (* push $ra, push $fp, $fp = $sp *)
    | FRAME_EXIT (* pop $fp, pop $ra *)

  and op2 =
    (* arith *)
    | ADD
    | SUB
    | MUL
    | DIV
    | MOD
    (* bits *)
    | SHL
    | SHR
    | LSHR
    (* logic *)
    | AND
    | OR
    | XOR
    | XNOR
    | NAND
    | NOR

  and op1 =
    (* arith *)
    | NEG
    (* bits *)
    | COMPL (* bitwise invert *)
    | SHLI of int (* shift at constant *)
    | SHRI of int (* shift at constant *)
    | LSHRI of int (* shift at constant *)
    (* logic *)
    | NOT

  (** helper function for LOAD_ARG *)
  let get_arg_offset (nth : int) (framerec : frame_record) =
    let rec r acc nth l =
      if nth = 0
      then acc
      else (
        match l with
        | hd :: tl -> r (acc + hd) (nth - 1) tl
        | _ -> assert false)
    in
    r 0 nth framerec.frame_params_sizes
  ;;
end
