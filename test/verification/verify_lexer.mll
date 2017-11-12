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

let end_example = ("end###" _+)

rule read = parse
  | [' ' '\t']+    {read lexbuf}
  | ['\n' '\r']    { next_line lexbuf; read lexbuf }
  | "."       { DOT }
  | "COMPILE" { COMPILE }
  | "RUN" { EXECUTE }
  | "EXPECT"  { EXPECT }
  | "STDOUT"  { STDOUT }
  | "STDERR"  { STDERR }
  | "TO"      { TO }
  | "EQUAL"   { EQUAL }
  | "CONTAIN" { CONTAIN }
  | "WITH"    { WITH }
  | "INPUT"   { INPUT }
  | "###example" { START_EXAMPLE }
  | end_example { END_EXAMPLE }
  | "---\n"
     { let buffer = Buffer.create 20 in
       STRING (stringl buffer lexbuf)
     }
  | "'"
     { let buffer = Buffer.create 20 in
       STRING (stringls buffer lexbuf)
     }
  | _              { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof            { EOF }
and stringl buffer = parse
  | "\n---" { Buffer.contents buffer }
  | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }
  | eof { raise (SyntaxError "Unexpected eof inside string") }
and stringls buffer = parse
  | "'" { Buffer.contents buffer }
  | _ as char { Buffer.add_char buffer char; stringls buffer lexbuf }
  | eof { raise (SyntaxError "Unexpected eof inside string") }
