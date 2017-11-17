open Core.Std
open Elang_lex
open Lexing
open Printf

module Parser = struct
  include Elang_parser.Incremental
  type ast = Ast.ast
  type 'a checkpoint = 'a Elang_parser.MenhirInterpreter.checkpoint
end

module Lexer = struct
  include Elang_lex
  type token = Elang_parser.token
end

module S = Syntax_analyzer.Make(Error_messages)(Elang_parser.MenhirInterpreter)(Parser)(Lexer)
module L = Loader.Make(FileResolver)(S)
module Elang = Language.Make(L)(Codegen)(Interpreter)

let loop print_module print_program no_color filename  () =
  ErrorReporter.set_color (not no_color);
  let options = Language.StringMap.of_alist_exn [
      ("print_module", print_module);
      ("print_program", print_program)
    ] in
  match Elang.execute options filename with
  | Ok v -> ()
  | Error s -> fprintf stderr "%s\n" s; exit 1

let () =
  Command.basic ~summary:"interpret an elang program"
    Command.Spec.(
      empty
      +> flag "-print-module" no_arg ~doc:" dump the module after loading"
      +> flag "-print-program" no_arg ~doc:" dump the program after compilation"
      +> flag "-no-color" no_arg ~doc:" disable colored output"
      +> anon ("filename" %: file)
    )
    loop
  |> Command.run
