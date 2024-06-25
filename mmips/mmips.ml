module Compiler = struct
  let mmips (content : string) : string =
    let lexbuf = Lexing.from_string content in
    let ast = Parse.program Lex.lexer lexbuf in
    let res = Compile.compile ast in
    let asm = Compile.result2string res in
    asm
  ;;
end
