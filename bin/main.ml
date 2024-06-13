let () =
  let filename = Sys.argv.(1) in
  let astl = Parse.Driver.run filename in
  (* astl |> Syntactics.Format.fmt stdout |> print_newline; *)
  (* astl
     |> List.map Syntactics.AST.sexp_of_expr
     |> List.iter (Sexplib.Sexp.output_hum stdout)
     |> print_newline *)
  try
    astl
    |> Semantics.Checking.check_module
    |> Ir_lisp.CC2CL.cl_of_module
    |> List.iter (Printf.printf "%a\n" Sexplib.Sexp.output_hum)
    |> print_newline
    (* |> List.iter (fun (n, ty) ->
       Printf.printf "%s: %s\n" n (Typing.MType.repr ty)) *)
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
