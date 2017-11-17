open Core.Std
open Lexing
open Printf

module type ErrorMessages = sig
  val message : int -> string
end

module type Lexer = sig
  type token
  val read : Lexing.lexbuf -> token
end

module type Parser = sig
  type ast
  type 'a checkpoint
  val prog : Lexing.position -> ast checkpoint
end

module Make
  (Error_messages : ErrorMessages)
  (IE : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE)  
  (Parser : Parser with type 'a checkpoint = 'a IE.checkpoint)
  (Lexer : Lexer with type token = IE.token) =
struct

  type ast = Parser.ast

  type filetype = FileResolver.filetype

  let pos_from_lexbuf lexbuf : Position.region =
    let open Position in
    make_region (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)

  let succeed (v : Parser.ast) = Ok v

  let fail lines lexbuf (checkpoint : 'a IE.checkpoint) =
    let region = pos_from_lexbuf lexbuf in
    match checkpoint with
    | IE.HandlingError env -> 
      let state_number = IE.current_state_number env in
      (try
        Error (ErrorReporter.report_error (Position.capture_pos region (Error_messages.message state_number)))
      with Not_found -> 
        Error (Printf.sprintf !"At %{ErrorReporter.Errors.print_position}: unknown syntax error in parser state: %d.%!" region state_number))
    | _ ->
      Error (Printf.sprintf !"At %{ErrorReporter.Errors.print_position}: unknown syntax error.%!" region)

  let loop lines lexbuf result =
    let supplier = IE.lexer_lexbuf_to_supplier Lexer.read lexbuf in
    IE.loop_handle succeed (fail lines lexbuf) supplier result
  
  let parse_file (file : FileResolver.filetype) =
    let open FileResolver in
    let text = In_channel.input_all file.chan in
    let lines = String.split_lines text in
    ErrorReporter.register_file file.filename lines;
    let lexbuf = from_string text in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file.filename };
    try
      loop lines lexbuf (Parser.prog lexbuf.lex_curr_p)
    with
    | Elang_lex.SyntaxError error ->
      Error (ErrorReporter.report_error error)
end
