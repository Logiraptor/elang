open Core.Std
open Elang_lex
open Lexing
open Printf

type module_ = Ast.ast

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_option lexbuf =
  Ok (Elang_parser.prog Elang_lex.read lexbuf)

let parse_with_error lexbuf =
  try parse_option lexbuf with
  | SyntaxError msg ->
    Result.Error (sprintf !"%{print_position}: %s\n" lexbuf msg)
  | Elang_parser.Error ->
    Result.Error (sprintf !"%{print_position}: syntax error\n" lexbuf)



(* Incremental parser below here *)

module I = Elang_parser.MenhirInterpreter

let succeed (v : Ast.ast) =
  (*The parser has succeeded and produced a semantic value. Print it. 
    Printf.printf "%d\n%!" v*)
  Ok v

let fail lexbuf (checkpoint : Ast.ast I.checkpoint) =
  match checkpoint with
  | I.HandlingError env -> 
    (try
       let state_number = I.current_state_number env in
       let message = Error_messages.message state_number in
       Error (Printf.sprintf "At %s: %s\n%!" (print_position lexbuf) message)
     with Not_found -> 
       Error (Printf.sprintf "At %s: unknown syntax error.\n%!" (print_position lexbuf)))
  | _ ->
    Error (Printf.sprintf "At %s: syntax error.\n%!" (print_position lexbuf))

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Elang_lex.read lexbuf in
  I.loop_handle succeed (fail lexbuf) supplier result

let load_module filename =
  let chan = In_channel.create filename in
  let text = In_channel.input_all chan in
  let lexbuf = from_string text in
  let _ = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } in
  try
    loop lexbuf (Elang_parser.Incremental.prog lexbuf.lex_curr_p)
  with
  | Elang_lex.SyntaxError msg ->
    Error (Printf.sprintf "%s%!" msg)


(*Old load_module*)
(*let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  let _ = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } in
  let output = parse_with_error lexbuf in
  let _ = In_channel.close inx in
  output*)