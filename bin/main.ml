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
  (* astl |> Syntactics.Format.fmt stdout |> print_newline; *)
  (* astl
     |> List.map Syntactics.AST.sexp_of_expr
     |> List.iter (Sexplib.Sexp.output_hum stdout)
     |> print_newline *)
  try
    astl
    |> Semantics.Checking.check_module
    |> List.iter (fun (n, ty) ->
      Printf.printf "%s: %s\n" n (Typing.MType.repr ty))
  with
  | Semantics.Checking.TypeError (msg, rng) ->
    Printf.eprintf
      "SemanticError: %s at %a\n"
      msg
      Lexical.Range.dump
      rng
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
