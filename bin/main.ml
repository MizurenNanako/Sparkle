open Lexical

let run_lexer_only () =
  let fname = Sys.argv.(1) in
  let lex = Lexer.init fname in
  let rec l () =
    let tk = lex () in
    match tk with
    | Teof ->
      Teof |> Token.show_token |> print_endline;
      lex ()
    | tk ->
      tk |> Token.show_token |> print_endline;
      l ()
  in
  l ()
;;

let run_parse_only () =
  let ast = Parse.Driver.run_file Sys.argv.(1) in
  Printf.printf "%s\n" (Syntactics.AST.show ast)
;;

let run_parse_check () =
  let ast = Parse.Driver.run_file Sys.argv.(1) in
  try
    let ast, env = ast |> Semantics.Check.check_translation_unit in
    ast |> List.map Syntactics.AST2.show_toplevel |> List.iter print_endline;
    env |> Typing.Env.show_env |> print_endline
    (* Typing.Env.dirty_table |> Typing.Env.show_dirty_table |> print_endline *)
  with
  | Semantics.Check.TypeError (msg, rng) ->
    Printf.eprintf "TypeError: %s at %s\n" msg (Lexical.Range.show rng)
;;

(* let run_compile_to_cish () =
   let ast = Parse.Driver.run_file Sys.argv.(1) in
   try
   let ast, _env = ast |> Semantics.Check.check_translation_unit in
   ast |> List.map Ir_cnv.Cish_cnv.transpile_toplevel |> List.iter print_endline
   with
   | Semantics.Check.TypeError (msg, rng) ->
   Printf.eprintf "TypeError: %s at %s\n" msg (Lexical.Range.show rng)
   ;; *)

let run_compile_to_stackir () =
  let ast = Parse.Driver.run_file Sys.argv.(1) in
  try
    let ast, env = ast |> Semantics.Check.check_translation_unit in
    Ir_cnv.Stack_ir_cnv.set_env env;
    ast |> List.iter Ir_cnv.Stack_ir_cnv.cc_toplevel;
    Ir_cnv.Stack_ir_cnv.frame_tab
    |> Stack_ir.StkIR.show_frame_table
    |> print_endline;
    Ir_cnv.Stack_ir_cnv.cst_tab |> Stack_ir.StkIR.show_data_table |> print_endline
  with
  | Semantics.Check.TypeError (msg, rng) ->
    Printf.eprintf "TypeError: %s at %s\n" msg (Lexical.Range.show rng)
;;

let run_compile_to_mips () =
  let ast = Parse.Driver.run_file Sys.argv.(1) in
  try
    let ast, env = ast |> Semantics.Check.check_translation_unit in
    Ir_cnv.Stack_ir_cnv.set_env env;
    ast |> List.iter Ir_cnv.Stack_ir_cnv.cc_toplevel;
    Ir_cnv.Stack_ir_cnv.frame_tab
    |> Stack_ir.ToMips.cc_all
    |> Printf.printf "\t.text\n\t.align\t2\n\t.globl main\n%s\n";
    Ir_cnv.Stack_ir_cnv.cst_tab
    |> Stack_ir.ToMips.cc_data
    |> Printf.printf "\n\t.data\n%s\n";
    "prim.asm"
    |> In_channel.open_text
    |> In_channel.input_all
    |> Printf.printf "\n\t.text\n%s\n"
  with
  | Semantics.Check.TypeError (msg, rng) ->
    Printf.eprintf "TypeError: %s at %s\n" msg (Lexical.Range.show rng)
;;

let () =
  ignore
    ( run_lexer_only
    , run_parse_only
    , run_parse_check
    , run_compile_to_stackir
    , run_compile_to_mips );
  (* run_lexer_only (); *)
  (* run_parse_only () *)
  (* run_parse_check (); *)
  (* run_compile_to_stackir () *)
  run_compile_to_mips ()
;;
(* run_compile_to_cish () *)
