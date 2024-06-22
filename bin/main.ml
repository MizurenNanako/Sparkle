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
  try ast |> Semantics.Check.check_translation_unit with
  | Semantics.Check.TypeError (msg, rng) ->
    Printf.eprintf "TypeError: %s at %s\n" msg (Lexical.Range.show rng)
;;

let () =
  ignore (run_lexer_only, run_parse_only, run_parse_check);
  (* run_lexer_only (); *)
  (* run_parse_only () *)
  run_parse_check ()
;;
