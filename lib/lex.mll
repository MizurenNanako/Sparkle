(* header section *)
{
  open Parse
  open Lexing
}

(* definition section *)
let cr='\r'
let nl='\n'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9'] 

let id = ['A'-'Z''a'-'z''_']['a'-'z''A'-'Z''0'-'9''_']*

(* rules section *)

rule lexer = parse
| eol { Lexing.new_line lexbuf; lexer lexbuf } 
| ws+ { lexer lexbuf }
| ';' { SEMI }
| '{' { LBRACE }
| '}' { RBRACE }
| '(' { LPAREN } 
| ')' { RPAREN }
| "==" { EQEQ }
| "=" { EQ }
| "!=" { NEQ }
| "<=" { LTE }
| ">=" { GTE }
| '<' { LT }
| '>' { GT }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| "&&" { AND }
| "||" { OR }
| '!' { BANG }
| ',' { COMMA }
| "return" { RETURN }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| "let" { LET }
| id { ID(Lexing.lexeme lexbuf) }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) } 
| "/*" { comment lexbuf } (* comment start *)
| eof { EOF }
| _ as c 
{
    raise @@ 
      Failure(
        Printf.sprintf "unsupport charactor at \"%s\" line %i: %c" 
          lexbuf.lex_curr_p.pos_fname
          lexbuf.lex_curr_p.pos_lnum
          c
      ) 
}

and comment = parse 
| eol { Lexing.new_line lexbuf; comment lexbuf }
| "*/" { lexer lexbuf } (* comment end *)
| _ { comment lexbuf }

(* trailer section *)
