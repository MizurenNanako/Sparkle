module Driver = struct
  module A = Syntactics.AST
  module T = Lexical.Token
  module R = Lexical.Range
  module I = Parser.MenhirInterpreter

  let rec _run2 lex last (checkpoint : A.t I.checkpoint) : A.t =
    match checkpoint with
    | Accepted v -> v
    | Rejected ->
      let rng = Lexer.start_p (), Lexer.curr_p () in
      Printf.eprintf "SyntaxError: %s\n" (R.show rng);
      exit (-1)
    | AboutToReduce _ ->
      let next = I.resume checkpoint in
      _run2 lex last next
    | Shifting _ ->
      let next = I.resume checkpoint in
      _run2 lex last next
    | HandlingError _ ->
      (* let next = I.resume checkpoint in *)
      let rng = Lexer.start_p (), Lexer.curr_p () in
      Printf.eprintf "SyntaxError: %s\n" (R.show rng);
      exit (-1)
    | InputNeeded _ ->
      let tk = lex () in
      let l, r = T.getrng tk in
      let next = I.offer checkpoint (tk, l, r) in
      _run2 lex checkpoint next
  ;;

  let run_file (filename : string) : A.t =
    let lex = Lexer.init filename in
    let startpoint = Parser.Incremental.start (Lexer.curr_p ()) in
    _run2 lex startpoint startpoint
  ;;
end
