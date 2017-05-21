open Core.Std
open Elang_lex
open Lexing
open Printf

module L = Loader.Make(FileResolver)(Syntax_analyzer)
module Elang = Language.Make(L)(Codegen)(Interpreter)

let loop print_module print_program filename  () =
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
      +> anon ("filename" %: file)
    )
    loop
  |> Command.run
