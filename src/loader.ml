open Core.Std
open Lexing
open Printf

type typ = 
  | Int
  | Arrow of (typ * typ)

type inner_module = {
  ast : Ast.ast;
  types : typ Ast.SymbolTable.t
}

module Make
    (FR : Language.FileResolver)
    (P : Language.Parser with type ast = Ast.ast and type filetype = FR.filetype) = 
struct

  type module_ = inner_module


  let type_check m =
    Ok {ast = m; types = Ast.SymbolTable.empty}


  let load_module filename =
    let open Result in
    FR.resolve filename
    >>= P.parse_file
    >>= type_check

end
