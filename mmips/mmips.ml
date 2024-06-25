module Compiler = struct
  let mmips lexbuf : string =
    try
      let ast = Parse.program Lex.lexer lexbuf in
      let res = Compile.compile ast in
      let asm = Compile.result2string res in
      asm
    with
    | Failure msg ->
      prerr_endline msg;
      exit (-1)
    | _ ->
      Printf.eprintf
        "Syntax Error at \"%s\" line: %i\n"
        lexbuf.lex_curr_p.pos_fname
        lexbuf.lex_curr_p.pos_lnum;
      exit (-1)
  ;;
end
