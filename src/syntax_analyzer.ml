open Core.Std
open Elang_lex
open Lexing
open Printf

type ast = Ast.ast

type filetype = FileResolver.filetype

module I = Elang_parser.MenhirInterpreter

let pos_from_lexbuf lexbuf : Ast.region =
  let open Ast in
  {startPos=Ast.to_pos (Lexing.lexeme_start_p lexbuf); endPos=Ast.to_pos (Lexing.lexeme_end_p lexbuf)}

let succeed (v : Ast.ast) = Ok v

let fail lines lexbuf (checkpoint : 'a I.checkpoint) =
  let region = pos_from_lexbuf lexbuf in
  match checkpoint with
  | I.HandlingError env -> 
    let state_number = I.current_state_number env in
    (try
       Error (ErrorReporter.report_error region (Error_messages.message state_number))
     with Not_found -> 
       Error (Printf.sprintf !"At %{ErrorReporter.Errors.print_position}: unknown syntax error in parser state: %d.%!" region state_number))
  | _ ->
    Error (Printf.sprintf !"At %{ErrorReporter.Errors.print_position}: unknown syntax error.%!" region)

let loop lines lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Elang_lex.read lexbuf in
  I.loop_handle succeed (fail lines lexbuf) supplier result

let parse_file (file : FileResolver.filetype) =
  let open FileResolver in
  let text = In_channel.input_all file.chan in
  let lines = String.split_lines text in
  ErrorReporter.register_file file.filename lines;
  let lexbuf = from_string text in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file.filename };
  try
    loop lines lexbuf (Elang_parser.Incremental.prog lexbuf.lex_curr_p)
  with
  | Elang_lex.SyntaxError msg ->
    Error (Printf.sprintf "%s%!" msg)

