module Driver = struct
  module I = Parser.MenhirInterpreter
  module A = Syntactics.AST
  module Msg = Error_messages

  let rec _run
    (lexbuf : Lexing.lexbuf)
    (checkpoint : A.expr list I.checkpoint)
    (lastpoint : A.expr list I.checkpoint)
    =
    match checkpoint with
    | I.Accepted v -> v
    | I.Rejected -> []
    | I.AboutToReduce _ ->
      let cp = I.resume checkpoint in
      _run lexbuf cp lastpoint
    | I.Shifting _ ->
      let cp = I.resume checkpoint in
      _run lexbuf cp lastpoint
    | I.InputNeeded _ ->
      let tk =
        try Lexer.get_token lexbuf with
        | Lexical.Error.LexicalError (msg, _) ->
          Printf.eprintf
            "LexicalError: %s at %a\n"
            msg
            Lexical.Range.dump
            (lexbuf |> Lexical.Range.of_lexbuf);
          exit 0
      in
      let pL = lexbuf.lex_start_p in
      let pR = lexbuf.lex_curr_p in
      let cp = I.offer checkpoint (tk, pL, pR) in
      _run lexbuf cp checkpoint
    | I.HandlingError e ->
      let state_num = I.current_state_number e in
      let msg = Msg.message state_num in
      Printf.eprintf
        "SyntaxError: at %a\n%s"
        Lexical.Range.dump
        (lexbuf |> Lexical.Range.of_lexbuf)
        msg;
      []
  ;;

  let run (filename : string) : A.expr list =
    let file = In_channel.open_text filename in
    let lexbuf = Lexing.from_channel file in
    Lexing.set_filename lexbuf filename;
    let startpoint = Parser.Incremental.start lexbuf.lex_curr_p in
    _run lexbuf startpoint startpoint
  ;;
end
