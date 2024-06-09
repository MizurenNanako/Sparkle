let () =
  let filename = Sys.argv.(1) in
  let file = In_channel.open_text filename in
  let lexbuf = Lexing.from_channel file in
  Lexing.set_filename lexbuf filename;
  let astl =
    try Parser.start Lexer.get_token lexbuf with
    | e ->
      lexbuf
      |> Lexical.Range.of_lexbuf
      |> Lexical.Range.dump stdout
      |> print_newline;
      raise e
  in
  astl
  |> List.map Syntactics.AST.sexp_of_expr
  |> List.iter (Sexplib.Sexp.output_hum stdout)
  |> print_newline
;;

(* let () =
   let filename = Sys.argv.(1) in
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
   loop () *)