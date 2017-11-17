{
open Lexing
open Elang_parser

exception SyntaxError of string Position.with_pos

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

let rec next_n_lines n lexbuf =
  match n with
  | 0 -> lexbuf
  | n -> next_line lexbuf; next_n_lines (n - 1) lexbuf

let makeError (lexbuf: Lexing.lexbuf) (message: string) : string Position.with_pos =
  let s = lexeme_start_p lexbuf in
  let e = lexeme_end_p lexbuf in
  let region = Position.make_region s e in
  Position.capture_pos region message

let advance_lines comment lexbuf =
  let lines = Core.Std.String.split ~on:'\n' comment in
  next_n_lines ((List.length lines) - 1) lexbuf

}

let int = '-'? ['0'-'9'] ['0'-'9']*

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let comment = '#' ([^ '\n']+)
let multiline_comment = "###" ([^'#']|('#'[^'#'])|("##"[^'#']))+ "###"

rule read = parse
  | [' ' '\t']+    {read lexbuf}
  | ['\n' '\r']    { next_line lexbuf; read lexbuf }
  | "let"          { LET }
  | "type"         { TYPE }
  | "in"           { IN }
  | "if"           { IF }
  | "then"         { THEN }
  | "else"         { ELSE }
  | "extern"       { EXTERN }
  | "struct"       { STRUCT }
  | "."            { DOT }
  | "="            { EQUAL }
  | ","            { COMMA }
  | "("            { LPAREN }
  | ")"            { RPAREN }
  | "{"            { LCURLY }
  | "}"            { RCURLY }
  | "+"            { PLUS }
  | "*"            { TIMES }
  | "-"            { MINUS }
  | "%"            { MOD }
  | "&"            { AND }
  | multiline_comment as cmt { advance_lines cmt lexbuf; read lexbuf }
  | comment        { read lexbuf }
  | int as lexeme  { INT (int_of_string lexeme) }
  | id as lexeme   { ID lexeme }
  | '"'
     { let buffer = Buffer.create 20 in
       STRING (stringl buffer lexbuf)
     }
  | _              { raise (SyntaxError (makeError lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf))) }
  | eof            { EOF }
and stringl buffer = parse
  | '"' { Buffer.contents buffer }
  | "\\t" { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
  | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
  | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
  | '\\' '"' { Buffer.add_char buffer '"'; stringl buffer lexbuf }
  | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
  | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }
  | eof { raise (SyntaxError (makeError lexbuf "Unexpected eof inside string")) }
