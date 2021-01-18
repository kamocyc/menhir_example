{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1 }
}

let space = '\t' | ' '
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let frac = '.' digit+
let float = digit+ frac?
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
| space { token lexbuf }
| newline { next_line lexbuf; token lexbuf }
| float { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
| "null" { NULL }
| '{' { LEFT_BRACE }
| '}' { RIGHT_BRACE }
| ':' { COLON }
| ',' { COMMA }
| id { ID (Lexing.lexeme lexbuf) }
| eof { EOF }
| _ { raise (SyntaxError "error")}