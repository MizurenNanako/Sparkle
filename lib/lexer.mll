{
    open Lexical.Error
    open Lexical.Token
    open Lexical.Literal
    open Lexical

    let lbstk = Stack.create ()
    let lb () = Stack.top lbstk
    
    let _run fname = 
        let fch = In_channel.open_text fname in
        let lexbuf = Lexing.from_channel fch in
        Lexing.set_filename lexbuf fname;
        Stack.push lexbuf lbstk

    let new_line () = 
        let lbb = Stack.pop lbstk in
        Lexing.new_line lbb;
        Stack.push lbb lbstk
}

let punct         = ['(' ')' '[' ']' '{' '}' '<' '>' 
                     ',' '|' '?' '+' '-' '*' '/' '.'
                     '!' ':' '=' '"' '`' '\n']
let ws            = [' ' '\t']
let num           = ['0'-'9']
let identifier    = (_ # punct # num # ws) (_ # punct # ws)*
let oct_char      = ['0'-'7']
let hex_char      = ['0'-'9' 'A'-'F' 'a'-'f']
(* let numbody       = ['0'-'9'] *)
let numbody       = ['0'-'9' '\'']
let literal_dec   = ['1'-'9'] numbody* | '0'
(* let literal_dec   = ['1'-'9'] numbody* *)
let literal_oct   = '0' ['0'-'9']+
let literal_hex   = '0' ['x' 'X'] hex_char*
let literal_bin   = '0' ['b' 'B'] ['0' '1']*
(* let wholenumber   = ['+' '-']? ['1'-'9'] numbody* *)
let wholenumber   = ['+' '-']? numbody+
let fraction      = numbody+
let significand   = (wholenumber "." fraction) | ("." fraction) | (wholenumber ".")
let exponent      = ['e' 'E' 'p' 'P'] ['+' '-']? ['0'-'9']+
(* let literal_real  = (significand exponent? | wholenumber exponent) *)

rule get_token = parse
| '#' [^ '\n']* { get_token (lb ()) }
| [' ' '\t']+ { get_token (lb ()) }
| '\n' { new_line (); get_token (lb ()) }
| '`' ([^ '\n']* as fn)
{
    let dirpath = Filename.dirname (lb ()).lex_curr_p.pos_fname in
    _run (dirpath ^ "/" ^ fn); get_token (lb ()) 
}
| "\"" 
{
    let stpos = (lb ()).lex_start_p in
    let s = get_str (Buffer.create 17) (lb ()) in
    let edpos = (lb ()).lex_curr_p in
    let r = stpos, edpos in
    Tstr (s, r)
}

| "("    { Tlp (lb ()).lex_start_p }
| ")"    { Trp (lb ()).lex_start_p }
| "["    { Tlb (lb ()).lex_start_p }
| "]"    { Trb (lb ()).lex_start_p }
| "{"    { Tlc (lb ()).lex_start_p }
| "}"    { Trc (lb ()).lex_start_p }
| ","    { Tcomma (lb ()).lex_start_p }
| "|"    { Tbar (lb ()).lex_start_p }
| "?"    { Tquestion (lb ()).lex_start_p }
| "+"    { Tadd (lb ()).lex_start_p }
| "-"    { Tsub (lb ()).lex_start_p }
| "*"    { Tmul (lb ()).lex_start_p }
| "/"    { Tdiv (lb ()).lex_start_p }
| "^"    { Tsup (lb ()).lex_start_p }
| "."    { Tdot (lb ()).lex_start_p }
| ":"    { Tcolon (lb ()).lex_start_p }
| ";"    { Tsemi (lb ()).lex_start_p }
| "="    { Teq (lb ()).lex_start_p }
| "<"    { Tlt (lb ()).lex_start_p }
| ">"    { Tgt (lb ()).lex_start_p }
| ":="   { Tassign (lb ()).lex_start_p }
| "=="   { Tpeq (lb ()).lex_start_p }
| "!="   { Tpneq (lb ()).lex_start_p }
| "<>"   { Tneq (lb ()).lex_start_p }
| "<="   { Tleq (lb ()).lex_start_p }
| ">="   { Tgeq (lb ()).lex_start_p }
| "=>"   { Tinduce (lb ()).lex_start_p }
| "->"   { Tto (lb ()).lex_start_p }
| "or"   { Tor (lb ()).lex_start_p }
| "nil"  { Tnil (lb ()).lex_start_p }
| "and"  { Tand (lb ()).lex_start_p }
| "not"  { Tnot (lb ()).lex_start_p }
| "xor"  { Txor (lb ()).lex_start_p }
| "shl"  { Tshl (lb ()).lex_start_p }
| "shr"  { Tshr (lb ()).lex_start_p }
| "lshr" { Tlshr (lb ()).lex_start_p }
| "xnor" { Txnor (lb ()).lex_start_p }

| eof { Teof }

(* | (literal_real as s) { Tf64 (float_of_string s, Range.of_(lb ()) (lb ())) } *)
| (literal_dec as s) { Tint ((int_of_dec s).data, Range.of_lexbuf (lb ())) }
| (literal_oct as s) { Tint ((int_of_oct s).data, Range.of_lexbuf (lb ())) }
| (literal_hex as s) { Tint ((int_of_hex s).data, Range.of_lexbuf (lb ())) }
| (literal_bin as s) { Tint ((int_of_bin s).data, Range.of_lexbuf (lb ())) }
| identifier as id { Tid (id, Range.of_lexbuf (lb ())) }
| eof 
{
    if Stack.is_empty lbstk then Teof
    else ( ignore (Stack.pop lbstk); get_token (lb ()) )
}
| _ as c 
{ 
    raise @@ 
    LexicalError (
        Printf.sprintf "unexpected charactor: %c" c, 
        (lb ()).lex_curr_p) 
}

and get_str buf = parse
| '\"' { Buffer.contents buf }
| "\\t" { Buffer.add_char buf '\t'; get_str buf (lb ()) }
| "\\n" { Buffer.add_char buf '\n'; get_str buf (lb ()) }
| "\\\"" { Buffer.add_char buf '\"'; get_str buf (lb ()) }
| eof { raise @@ LexicalError ("unterminated string", (lb ()).lex_curr_p) }
| _ as c { Buffer.add_char buf c; get_str buf (lb ()) }

{
    let init fname = 
        _run fname; fun () -> try get_token (lb ()) with Stack.Empty -> Teof
}