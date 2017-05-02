
type symbol =
  string

type expr = 
  | INT of int

type func =
  (symbol * symbol list * expr)

type prog =
  func list

type ast =
  prog

module SymbolTable = struct
  include Map.Make(String)
end
