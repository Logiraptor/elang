open Core.Std
open Elang_lex
open Lexing
open Printf


type ast = Ast.ast

type filetype = FileResolver.filetype

module I = Elang_parser.MenhirInterpreter

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let number_line i text =
  (sprintf "%d: %s" (i+1) text)

let rec numbered i lines =
  match lines with
  | [] -> []
  | h::t -> 
    (number_line i h)::(numbered (i+1) t)


let lines_after lines i =
  let l = List.length lines in
  let start_pos = max 0 i in
  let end_pos = min (i+3) l in
  List.slice lines start_pos end_pos
  |> numbered start_pos

let lines_before lines i =
  let l = List.length lines in
  let start_pos = max (i-3) 0 in
  let end_pos = min l i in
  List.slice lines start_pos end_pos
  |> numbered start_pos

let line_with_ptr start_pos end_pos =
  let lhs = String.make start_pos ' ' in
  let rhs = "" in
  let ptr = ANSITerminal.(sprintf [red; Bold]) "%s" (String.make (end_pos - start_pos) '^') in
  lhs ^ ptr ^ rhs

let get_context lines lexbuf : string list  = 
  let start_pos = Lexing.lexeme_start_p lexbuf in
  let start_line_offset = start_pos.pos_cnum - start_pos.pos_bol in
  let end_pos = Lexing.lexeme_end_p lexbuf in
  let end_line_offset = end_pos.pos_cnum - end_pos.pos_bol in
  let lnum = start_pos.pos_lnum - 1 in
  let Some line = List.nth lines lnum in
  let line_with_number = number_line lnum line in
  let left_padding_due_to_number = (String.length line_with_number) - (String.length line) in
  let bef = lines_before lines lnum in
  let aft = lines_after lines (lnum+1) in
  let ptr = line_with_ptr start_line_offset end_line_offset in
  let ptr_with_padding = (String.make left_padding_due_to_number ' ') ^ ptr in
  List.concat [bef; [line_with_number; ptr_with_padding]; aft]


let print_error_block (location : string) (inner : string list) : string =
  let (width, height) = ANSITerminal.size () in
  let right_block = String.make 16 '-' in
  let left_block = String.make 4 '-' in
  let header = left_block ^ location ^ right_block in
  let footer = String.make ((String.length header) - 9) '-' in
  let body = String.concat ~sep:"\n" inner in
  String.concat ~sep:"\n" [header; body; footer]


let print_error lines lexbuf state =
  let message = ANSITerminal.(sprintf [Bold]) "%s" (Error_messages.message state) in
  let location = ANSITerminal.(sprintf [blue]) "%s" (print_position lexbuf) in
  let context = get_context lines lexbuf in
  let block =  print_error_block location context in
  String.concat [message; block]


let succeed (v : Ast.ast) = Ok v

let fail lines lexbuf (checkpoint : Ast.ast I.checkpoint) =
  match checkpoint with
  | I.HandlingError env -> 
    let state_number = I.current_state_number env in
    (try
       Error (print_error lines lexbuf state_number)
     with Not_found -> 
       Error (Printf.sprintf !"At %{print_position}: unknown syntax error in parser state: %d.%!" lexbuf state_number))
  | _ ->
    Error (Printf.sprintf !"At %{print_position}: unknown syntax error.%!" lexbuf)

let loop lines lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Elang_lex.read lexbuf in
  I.loop_handle succeed (fail lines lexbuf) supplier result

let parse_file (file : FileResolver.filetype) =
  let open FileResolver in
  let text = In_channel.input_all file.chan in
  let lines = String.split_lines text in
  let lexbuf = from_string text in
  let _ = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file.filename } in
  try
    loop lines lexbuf (Elang_parser.Incremental.prog lexbuf.lex_curr_p)
  with
  | Elang_lex.SyntaxError msg ->
    Error (Printf.sprintf "%s%!" msg)

