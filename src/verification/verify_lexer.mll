{
open Lexing
open Verify_parser

exception SyntaxError of string

let getError lexbuf =
    begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        (line, cnum, tok)
      end

let string_of_error (line, column, token) =
    Printf.sprintf "%d:%d %s" line column token

let next_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let comment = '#' ([^ '\n']+)

rule read = parse
  | [' ' '\t']+    {read lexbuf}
  | ['\n' '\r']    { next_line lexbuf; read lexbuf }
  | "EXPECT"        { EXPECT }
  | "COMPILATION"   { COMPILATION }
  | "TO"            { TO }
  | "PRINT"         { PRINT }
  | "ERROR"         { ERROR }
  | "EXECUTION"     { EXECUTION }
  | "WITH"          { WITH }
  | "INPUT"         { INPUT }
  | '"'
     { let buffer = Buffer.create 20 in
       STRING (stringl buffer lexbuf)
     }
  | _              { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof            { EOF }
and stringl buffer = parse
  | '"' { Buffer.contents buffer }
  | '\\' '"' { Buffer.add_char buffer '"'; stringl buffer lexbuf }
  | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
  | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }
  | eof { raise (SyntaxError (makeError lexbuf "Unexpected eof inside string")) }
