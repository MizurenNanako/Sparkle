let run_fmt filename =
  let astl = Parse.Driver.run filename in
  astl |> Syntactics.Format.fmt stdout |> print_newline
;;

let run_parser filename =
  let astl = Parse.Driver.run filename in
  astl
  |> List.map Syntactics.AST.sexp_of_expr
  |> List.iter (Sexplib.Sexp.output_hum stdout)
  |> print_newline
;;

let run_cl filename =
  let astl = Parse.Driver.run filename in
  try
    astl
    |> Semantics.Checking.check_module
    |> Ir_lisp.CC2CL.cl_of_module
    |> Sexplib.Sexp.output_hum stdout
    |> print_newline
  with
  | Semantics.Checking.TypeError (msg, rng) ->
    Printf.eprintf
      "SemanticError: %s at %a\n"
      msg
      Lexical.Range.dump
      rng
;;

let run_lexer filename =
  let file = In_channel.open_text filename in
  let lexbuf = Lexing.from_channel file in
  Lexing.set_filename lexbuf filename;
  let rec loop () =
    let tok = Lexer.get_token lexbuf in
    match tok with
    | Lexical.Token.Teof -> ()
    | _ ->
      tok |> Lexical.Token.to_str |> Printf.printf "%s\n";
      loop ()
  in
  loop ()
;;

let usage_msg = "spml -[lex|parse|format|cl] <file>"

(* let usage_msg = "spml -[lex|parse|format|cl|lambda] <file>" *)
let mode_lex = ref false
let mode_parse = ref false
let mode_format = ref false
let mode_cl = ref true
(* let mode_lambda = ref false *)
let input_file = ref ""
let anon_fun filename = input_file := filename

let speclist =
  [ "-lex", Arg.Set mode_lex, "Output lex information"
  ; "-parse", Arg.Set mode_parse, "Output parse information"
  ; "-format", Arg.Set mode_format, "Output formatted input"
  ; "-cl", Arg.Set mode_cl, "transpile to common lisp"
    (* ; "-lambda", Arg.Set mode_cl, "transpile to pure lambda calculus" *)
  ]
;;

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !mode_lex
  then run_lexer !input_file
  else if !mode_parse
  then run_parser !input_file
  else if !mode_format
  then run_fmt !input_file
  else if !mode_cl
  then run_cl !input_file
;;
