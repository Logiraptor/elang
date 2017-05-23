open Core.Std
open Lexing
open Printf

module Errors = struct
  type context = {
    before : string list;
    pointer : string;
    after : string list;
  }

  type error_message = {
    message : string;
    location : Ast.region;
    context : context;
  }

  open Ast
  let print_position ({startPos; endPos} : Ast.region) =
    sprintf "%s:%d:%d-%d"
      startPos.filename
      startPos.linenum
      (startPos.beginning_of_token - startPos.beginning_of_line + 1)
      (endPos.beginning_of_token - endPos.beginning_of_line + 1)

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
    let ptr = String.make (end_pos - start_pos) '^' in
    lhs ^ ptr ^ rhs

  let get_context lines ({startPos; endPos} : Ast.region) : context  = 
    let start_line_offset = startPos.beginning_of_token - startPos.beginning_of_line in
    let end_line_offset = endPos.beginning_of_token - endPos.beginning_of_line in
    let lnum = startPos.linenum - 1 in
    let Some line = List.nth lines lnum in
    let line_with_number = number_line lnum line in
    let left_padding_due_to_number = (String.length line_with_number) - (String.length line) in
    let bef = lines_before lines lnum in
    let aft = lines_after lines (lnum+1) in
    let ptr = line_with_ptr start_line_offset end_line_offset in
    let ptr_with_padding = (String.make left_padding_due_to_number ' ') ^ ptr in
    { 
      before=List.concat [bef; [line_with_number]];
      pointer=ptr_with_padding;
      after=aft
    }

  let print_error_block (location : string) (inner : string list) color_len : string =
    let right_block = String.make 16 '-' in
    let left_block = String.make 4 '-' in
    let header = left_block ^ location ^ right_block in
    let footer = String.make ((String.length header) - color_len) '-' in
    let body = String.concat ~sep:"\n" inner in
    String.concat ~sep:"\n" [header; body; footer]

  (*let print_error lines (pos : Ast.region) message =
    let message = ANSITerminal.(sprintf [Bold]) "%s\n" (message) in
    let location = ANSITerminal.(sprintf [blue]) "%s" (print_position pos) in
    let context = get_context lines pos in
    let block =  print_error_block location context in
    String.concat [message; block]*)

  let print colored {context; location; message} =
    let open ANSITerminal in
    let sprintf style message = 
      if colored then 
        sprintf style "%s" message 
      else message 
    in
    let {before; pointer; after} = context in
    let location = sprintf [blue] (print_position location) in
    let message = sprintf [Bold] message in
    let pointer = sprintf [red;Bold] pointer in
    let contents = List.concat [before; [pointer]; after] in
    let block = print_error_block location contents (if colored then 9 else 0) in
    String.concat [message; block]

  let ensure_newline message =
    if String.is_suffix message ~suffix:"\n" then message else message ^ "\n"

  let print_error colored lines (pos : Ast.region) message =
    let context = get_context lines pos in
    print colored {context=context; location=pos; message=ensure_newline message}
end

type file = {filename : string; lines : string list}

let files = String.Table.create ()

let color = ref true

let set_color v =
  color := v

let report_error (pos : Ast.region) error =
  let open Ast in
  let lines = String.Table.find_exn files pos.startPos.filename in
  Errors.print_error (!color) lines pos error

let register_file name lines =
  Hashtbl.add_exn files ~key:name ~data:lines


