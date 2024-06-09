{
    open Lexical.Error
    open Lexical.Token
    open Lexical.Literal
    open Lexical
}

let reserved      = ['|' ':' '[' ']' '(' ')' '{' '}' ' ' '\t' '\n' ',' '?']
let num       = ['0'-'9']
let identifier    = (_ # reserved # num) (_ # reserved)*
let oct_char      = ['0'-'7']
let hex_char      = ['0'-'9' 'A'-'F' 'a'-'f']
(* let numbody       = ['0'-'9'] *)
let numbody       = ['0'-'9' '\'']
let literal_dec   = ['+' '-']? ['1'-'9'] numbody* | '0'
(* let literal_dec   = ['1'-'9'] numbody* *)
let literal_oct   = '0' ['0'-'9']+
let literal_hex   = '0' ['x' 'X'] hex_char*
let literal_bin   = '0' ['b' 'B'] ['0' '1']*
(* let wholenumber   = ['+' '-']? ['1'-'9'] numbody* *)
let wholenumber   = ['+' '-']? numbody+
let fraction      = numbody+
let significand   = (wholenumber "." fraction) | ("." fraction) | (wholenumber ".")
let exponent      = ['e' 'E' 'p' 'P'] ['+' '-']? ['0'-'9']+
let literal_real  = (significand exponent? | wholenumber exponent)

rule get_token = parse
| [' ' '\t']+ { get_token lexbuf }
| '\n' { Lexing.new_line lexbuf; get_token lexbuf }
| "\"" 
{
    let stpos = lexbuf.lex_start_p in
    let s = get_str (Buffer.create 17) lexbuf in
    let edpos = lexbuf.lex_curr_p in
    let r = stpos, edpos in
    Tstr (s, r)
}
| "|" { Tbar lexbuf.lex_start_p }
| ":" { Tcolon lexbuf.lex_start_p }
| "[" { Tlb lexbuf.lex_start_p }
| "]" { Trb lexbuf.lex_start_p }
| "(" { Tlp lexbuf.lex_start_p }
| ")" { Trp lexbuf.lex_start_p }
| "{" { Tlc lexbuf.lex_start_p }
| "}" { Trc lexbuf.lex_start_p }
(* | "?" { Tquest lexbuf.lex_start_p } *)
| "," { Tcomma lexbuf.lex_start_p }
| ":=" { Tbind lexbuf.lex_start_p }
| "->" { Tto lexbuf.lex_start_p }
| "=>" { Tin lexbuf.lex_start_p }
| (literal_real as s) { Tf64 (float_of_string s, Range.of_lexbuf lexbuf) }
| (literal_dec as s) { Ti64 ((int_of_dec s).data, Range.of_lexbuf lexbuf) }
| (literal_oct as s) { Ti64 ((int_of_oct s).data, Range.of_lexbuf lexbuf) }
| (literal_hex as s) { Ti64 ((int_of_hex s).data, Range.of_lexbuf lexbuf) }
| (literal_bin as s) { Ti64 ((int_of_bin s).data, Range.of_lexbuf lexbuf) }
| identifier as id { Tid (id, Range.of_lexbuf lexbuf) }
| eof { Teof }
| _ as c 
{ 
    raise @@ 
    LexicalError (
        Printf.sprintf "unexpected charactor: %c" c, 
        lexbuf.lex_curr_p) 
}

and get_str buf = parse
| '\"' { Buffer.contents buf }
| "\\t" { Buffer.add_char buf '\t'; get_str buf lexbuf }
| "\\n" { Buffer.add_char buf '\n'; get_str buf lexbuf }
| "\\\"" { Buffer.add_char buf '\"'; get_str buf lexbuf }
| eof { raise @@ LexicalError ("unterminated string", lexbuf.lex_curr_p) }
| _ as c { Buffer.add_char buf c; get_str buf lexbuf }
