open Lexical

let run_lexer_only () =
  let fname = Sys.argv.(1) in
  let lex = Lexer.init fname in
  let rec l () =
    let tk = lex () in
    match tk with
    | Teof -> ()
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

let () =
  ignore (run_lexer_only, run_parse_only);
  (* run_lexer_only () *)
  run_parse_only ()
;;
