open Core.Std
open Lexing
open Printf
open Result

type typ = 
  | Int
  (*| Arrow of (typ * typ)*)

type inner_module = {
  ast : Ast.ast;
  types : typ Ast.SymbolTable.t
}

module Make
    (FR : Language.FileResolver)
    (P : Language.Parser with type ast = Ast.ast and type filetype = FR.filetype) = 
struct

  type module_ = inner_module

  let load_module filename =
    let module_ast = FR.resolve filename >>= P.parse_file in
    match module_ast with
    | Ok ast -> Ok ({
        ast = ast;
        types = Ast.SymbolTable.empty
      })
    | Error err -> Error err

end
