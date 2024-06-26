module StkIR = struct
  exception NotImplanted

  (* note all instructions are for system word *)
  (* int32 on mips32 *)

  type word = int32 [@@deriving show { with_path = false }]

  let int_to_word = Int32.of_int
  let mul_word = Int32.mul
  let mul_word_with_int w i = mul_word w (int_to_word i)
  let i64_to_word = Int64.to_int32
  let str_of_word = Int32.to_string
  let size_of_word = 4l

  (* !!! ALL ADDR is not physical address, ther are phy_addr/sizeof(word) !!! *)

  (** the stkrec_addr is calculated with relative address,
      for (addr=n, size=1), it means offset($fp),
      where offset = (size_of_word * size)
      note that args are outside of frame!!! *)
  type stack_record =
    { stkrec_addr : int (* after a push, then set, so starts at 0, then -1 *)
    ; stkrec_size : int
    }
  [@@deriving show { with_path = false }]

  (* note that for a new frame, stack_addr is 0, but the first val will always be 1 *)

  let get_tmp_offset (stkrec : stack_record) =
    mul_word_with_int size_of_word stkrec.stkrec_addr
  ;;

  type segdata_record =
    { segd_symbol : string
    ; segd_raw : bytes
    }
  [@@deriving show { with_path = false }]

  (* all can be access with [List.assoc] *)
  type lookup_table = (string * stack_record) list ref
  [@@deriving show { with_path = false }]

  type data_table = (string * segdata_record) list ref
  [@@deriving show { with_path = false }]

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
     / just a marker
     0. PRECALL
     / alloc for return val
     1. ALLOC_RET frame_retval_size
     / eval an, ..., a2, a1
     2. do the work for an ... a1
     (* /. eval f, this can be a expr *)
     (* 3. do the work for f *)
     / store frame pointer, $ra, and something
     4. FRAME_ENTER
     5. CALL f
     / restore frame pointer, $ra, and something
     6. FRAME_EXIT
     7. POPN sum(frame_params_sizes)

     so in this model, functions do not clean by itself.
  *)

  (*
     consider a function body with frame_record
     1. just do the job
  *)

  type frame_record =
    { frame_params_sizes : int list (** size of arg1, arg2, ..., argn *)
    ; frame_retval_size : int (** size of return value *)
    ; frame_code : instruction list
    }
  [@@deriving show { with_path = false }]

  and frame_table = (string * frame_record) list ref
  [@@deriving show { with_path = false }]

  and instruction =
    | NOP (* do nothing *)
    | IM of word (* pushin an int value *)
    | POP (* remove top of stack *)
    | POPN of int (* pop n times *)
    | OP2 of op2 (* a<-pop, b<-pop, push<-(a op b) *)
    | OP1 of op1
    | BIND of string
      (* store name to lookup table without alloc,
         this uses unalloced space, but will be filled
         - after some exec. *)
    | ALLOC of string
    (* alloc for a name, the name and addr is then stored in [local_sym_table] *)
    | STORE_NAME of
        string (* store tos to the name, get size info from lookup table *)
    | LOAD_NAME of string (* load name to tos *)
    | LOAD_CONST of string (* load from .data to tos *)
    | LOAD_HIGH (* load $hi after mul/div *)
    | LOAD_LOW (* load $lo after mul/div *)
    | ALLOC_RET (* alloc 1 words for return value *)
    | STORE_RET (* ret is at $fp+argn+2 *)
    (* store return value *)
    (* store tos, tos+1, ..., to return value, respecting [frame_retval_size] *)
    | LABEL of string (* mark a label, for later jumping *)
    | J of string (* jump to label *)
    | JZ of string (* if tos = 0, jump to label *)
    | RET (* just return with nothing *)
    | PRECALL (* do nothing *)
    | CALL of string (* call to function with label from [frame_table] *)
    | LOAD_ARG of int
      (* load the inverted nth arg (start from 0), respecting [frame_params_sizes] *)
    | FRAME_ENTER (* push $ra, push $fp, $fp = $sp *)
    | FRAME_EXIT (* $sp = $fp, pop $fp, pop $ra *)

  and op2 =
    (* arith *)
    | ADD
    | ADDU
    | SUB
    | SUBU
    | MUL
    | MULU
    | DIV
    | DIVU
    | MOD
    | MODU
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
    (* compare, true for 1 *)
    | EQ
    | NEQ
    | LT
    | LEQ
    | GT
    | GEQ

  and op1 =
    (* arith *)
    | ADDI of word
    | SUBI of word
    | NEG
    (* bits *)
    | COMPL (* bitwise invert *)
    | SHLI of int (* shift at constant *)
    | SHRI of int (* shift at constant *)
    | LSHRI of int (* shift at constant *)
    (* logic *)
    | NOT

  (** helper function for LOAD_ARG
      NOTE: [offset * size_of_word = addr] *)
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

  (** for LOAD_NAME STORE_NAME
      NOTE: [offset * size_of_word = addr] *)
  let get_name_offset (name : string) (lookup_table : lookup_table) =
    let namerec = List.assoc name !lookup_table in
    match namerec with
    | { stkrec_addr; _ } -> stkrec_addr
  ;;

  (** no need to get its offset, asm will do it *)
  let get_const_name (name : string) (data_table : data_table) =
    let constrec = List.assoc name !data_table in
    match constrec with
    | { segd_symbol; _ } -> segd_symbol
  ;;
end

module ToMips = struct
  open StkIR

  let lktbl : lookup_table = ref []
  let sp_n = ref 0

  let lktbl_ins name addr size =
    lktbl := (name, { stkrec_addr = addr; stkrec_size = size }) :: !lktbl
  ;;

  let popt1 () =
    incr sp_n;
    [ "lw $t1, 0($sp)"; "addi $sp, $sp, 4" ]
  ;;

  let popt2 () =
    incr sp_n;
    [ "lw $t2, 0($sp)"; "addi $sp, $sp, 4" ]
  ;;

  let popt2t1 () = popt2 () @ popt1 ()

  let alloc1 () =
    decr sp_n;
    [ "addi $sp, $sp, -4" ]
  ;;

  let pusht1 () =
    decr sp_n;
    [ "addi $sp, $sp, -4"; "sw $t1, 0($sp)" ]
  ;;

  let pusht2 () =
    decr sp_n;
    [ "addi $sp, $sp, -4"; "sw $t2, 0($sp)" ]
  ;;

  let pushra () =
    decr sp_n;
    [ "addi $sp, $sp, -4"; "sw $ra, 0($sp)" ]
  ;;

  let popra () =
    incr sp_n;
    [ "lw $ra, 0($sp)"; "addi $sp, $sp, 4" ]
  ;;

  let pushfp () =
    decr sp_n;
    [ "addi $sp, $sp, -4"; "sw $fp, 0($sp)" ]
  ;;

  let popfp () =
    incr sp_n;
    [ "lw $fp, 0($sp)"; "addi $sp, $sp, 4" ]
  ;;

  let load_name (name : string) (lookup_table : lookup_table) =
    let addr =
      mul_word_with_int size_of_word (get_name_offset name lookup_table)
    in
    [ Printf.sprintf "lw $t1 %s($fp)" (str_of_word addr) ] @ pusht1 ()
  ;;

  let store_name (name : string) (lookup_table : lookup_table) =
    let addr =
      mul_word_with_int size_of_word (get_name_offset name lookup_table)
    in
    popt1 () @ [ Printf.sprintf "sw $t1 %s($fp)" (str_of_word addr) ]
  ;;

  let rec cc_instruction (frame : frame_record) (instruction : instruction) =
    match instruction with
    | NOP -> [ "nop" ]
    | IM i ->
      [ "li $t1, " ^ str_of_word i; "addi $sp, $sp, -4"; "sw $t1, 0($sp)" ]
    | POP ->
      incr sp_n;
      [ "addi $sp, $sp, 4" ]
    | POPN n ->
      sp_n := !sp_n + n;
      [ "addi $sp, $sp, " ^ string_of_int (4 * n) ]
    | OP2 op2 -> cc_op2 op2
    | OP1 op1 -> cc_op1 op1
    | BIND name ->
      lktbl_ins name (!sp_n - 1) 1;
      [ "nop" ]
    | ALLOC name ->
      lktbl_ins name (!sp_n - 1) 1;
      alloc1 ()
    | STORE_NAME name -> store_name name lktbl
    | LOAD_NAME name -> load_name name lktbl
    | LOAD_CONST name -> [ "la $t0, " ^ name; "lw $t1, 0($t0)" ] @ pusht1 ()
    | LOAD_HIGH -> [ "addi $t1, $hi, 0" ] @ pusht1 ()
    | LOAD_LOW -> [ "addi $t1, $lo, 0" ] @ pusht1 ()
    | ALLOC_RET -> alloc1 () @ []
    | STORE_RET ->
      let arg_offset_all = List.fold_left ( + ) 0 frame.frame_params_sizes in
      popt1 ()
      @ [ Printf.sprintf
            "sw $t1, %s($fp)"
            (arg_offset_all + 2 |> mul_word_with_int size_of_word |> str_of_word)
        ]
    | LABEL lbl -> [ lbl ^ ":" ]
    | J lbl -> [ "j " ^ lbl ]
    | JZ lbl -> popt1 () @ [ "beq $t1, $zero, " ^ lbl ]
    | RET -> [ "jr $ra" ]
    | PRECALL -> [ "nop" ]
    | CALL lbl -> [ "j " ^ lbl ]
    | LOAD_ARG n ->
      let arg_offset = get_arg_offset n frame in
      [ Printf.sprintf
          "lw $t1, %s($fp)"
          (mul_word_with_int size_of_word (arg_offset + 2) |> str_of_word)
      ]
    | FRAME_ENTER -> pushra () @ pushfp () @ [ "addi $fp, $sp, 0" ]
    | FRAME_EXIT -> [ "addi $sp, $fp, 0" ] @ popfp () @ popra ()

  and cc_op2 op2 =
    match op2 with
    | ADD -> popt2t1 () @ [ "add $t1, $t1, $t2" ] @ pusht1 ()
    | ADDU -> popt2t1 () @ [ "addu $t1, $t1, $t2" ] @ pusht1 ()
    | SUB -> popt2t1 () @ [ "sub $t1, $t1, $t2" ] @ pusht1 ()
    | SUBU -> popt2t1 () @ [ "subu $t1, $t1, $t2" ] @ pusht1 ()
    | MUL -> popt2t1 () @ [ "mul $t1, $t1, $t2" ] @ pusht1 ()
    | MULU -> popt2t1 () @ [ "mulu $t1, $t1, $t2" ] @ pusht1 ()
    | DIV -> popt2t1 () @ [ "div $t1, $t1, $t2" ] @ pusht1 ()
    | DIVU -> popt2t1 () @ [ "divu $t1, $t1, $t2" ] @ pusht1 ()
    | MOD -> popt2t1 () @ [ "div $t1, $t1, $t2"; "mfhi $t1" ] @ pusht1 ()
    | MODU -> popt2t1 () @ [ "modu $t1, $t1, $t2" ] @ pusht1 ()
    | SHL -> popt2t1 () @ [ "sllv $t1, $t1, $t2" ] @ pusht1 ()
    | SHR -> popt2t1 () @ [ "srav $t1, $t1, $t2" ] @ pusht1 ()
    | LSHR -> popt2t1 () @ [ "srlv $t1, $t1, $t2" ] @ pusht1 ()
    | AND -> popt2t1 () @ [ "and $t1, $t1, $t2" ] @ pusht1 ()
    | OR -> popt2t1 () @ [ "or $t1, $t1, $t2" ] @ pusht1 ()
    | XOR -> popt2t1 () @ [ "xor $t1, $t1, $t2" ] @ pusht1 ()
    | XNOR ->
      popt2t1 () @ [ "xor $t1, $t1, $t2"; "nor $t1, $t1, $zero" ] @ pusht1 ()
    | NAND ->
      popt2t1 () @ [ "and $t1, $t1, $t2"; "nor $t1, $t1, $zero" ] @ pusht1 ()
    | NOR -> popt2t1 () @ [ "nor $t1, $t1, $2" ] @ pusht1 ()
    | EQ ->
      popt2t1 () @ [ "subu $t1, $t1, $t2"; "nor $t1, $t1, $zero" ] @ pusht1 ()
    | NEQ -> popt2t1 () @ [ "subu $t1, $t1, $t2" ] @ pusht1 ()
    | LT -> popt2t1 () @ [ "slt $t1, $t1, $t2" ] @ pusht1 ()
    | LEQ -> popt2t1 () @ [ "slt $t1, $t2, $t1"; "xori $t1, $t1, 1" ] @ pusht1 ()
    | GT -> popt2t1 () @ [ "slt $t1, $t2, $t1" ] @ pusht1 ()
    | GEQ -> popt2t1 () @ [ "slt $t1, $t1, $t2"; "xori $t1, $t1, 1" ] @ pusht1 ()

  and cc_op1 op1 =
    match op1 with
    | ADDI i -> popt1 () @ [ "addi $t1, $t1, " ^ str_of_word i ] @ pusht1 ()
    | SUBI i -> popt1 () @ [ "subi $t1, $t1, " ^ str_of_word i ] @ pusht1 ()
    | NEG -> popt1 () @ [ "sub $t1, $zero, $t1" ] @ pusht1 ()
    | COMPL -> popt1 () @ [ "nor $t1, $t1, $zero" ] @ pusht1 ()
    | SHLI i -> popt1 () @ [ "sll $t1, $t1, " ^ string_of_int i ] @ pusht1 ()
    | SHRI i -> popt1 () @ [ "sra $t1, $t1, " ^ string_of_int i ] @ pusht1 ()
    | LSHRI i -> popt1 () @ [ "srl $t1, $t1, " ^ string_of_int i ] @ pusht1 ()
    | NOT -> popt1 () @ [ "nor $t1, $t1, $zero" ] @ pusht1 ()
  ;;

  let cc_frame (fname : string) (frame : frame_record) =
    let cclst = frame.frame_code |> List.concat_map (cc_instruction frame) in
    let cclst = (fname ^ ":") :: cclst in
    String.concat "\n\t" cclst
  ;;

  let cc_all (ftbl : frame_table) =
    !ftbl
    |> List.map (fun (name, frame) -> cc_frame name frame)
    |> String.concat "\n\n"
  ;;

  let cc_data (dtbl : data_table) =
    !dtbl
    |> List.map (fun (name, seg) ->
      Printf.sprintf
        "%s: .asciiz \"%s\""
        name
        (seg.segd_raw |> Bytes.escaped |> Bytes.to_string))
    |> String.concat "\n"
  ;;
end
