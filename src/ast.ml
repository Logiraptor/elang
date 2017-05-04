
type symbol =
  string

type op =
  | Add

type expr = 
  | Int of int
  | ID of symbol
  | BinOp of (op * expr * expr)
  | Apply of (expr * expr list)

type typ =
  | NamedType of symbol

type typed_symbol =
  (typ * symbol)

type func =
  (symbol * typed_symbol list * expr)

type prog =
  func list

type ast =
  prog

module SymbolTable = struct
  include Map.Make(String)
end
