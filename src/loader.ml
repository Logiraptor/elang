open Core.Std
open Elang_lex
open Lexing
open Printf


module Make
    (FR : Language.FileResolver)
    (P : Language.Parser) = 
struct

  type module_ = P.ast

  let load_module filename =
    let chan = FR.resolve filename in
    match P.parse_channel filename chan with
    | Ok ast -> Ok ast
    | Error err -> Error err

end
