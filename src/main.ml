open Core.Std
open Elang_lex
open Lexing
open Printf

module L = Loader.Make(FileResolver)(Syntax_analyzer)
module Elang = Language.Make(L)(Compiler)(Interpreter)

let loop filename () =
  match Elang.execute filename with
  | Ok v -> fprintf stdout !"%{Elang.string_of_value}" v
  | Error s -> fprintf stderr "%s\n" s

let () =
  Command.basic ~summary:"interpret an elang program"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop 
  |> Command.run
