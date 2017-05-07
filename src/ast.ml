open Core.Std

type symbol =
  string
[@@deriving sexp]

type op =
  | Add
  | Mul
  | Sub
  | Equal
[@@deriving sexp]

type expr = 
  | Int of int
  | ID of symbol
  | String of string
  | BinOp of (op * expr * expr)
  | Apply of (expr * expr list)
  | If of (expr * expr * expr)
[@@deriving sexp]

type typ =
  | NamedType of symbol
[@@deriving sexp]

type typed_symbol =
  (typ * symbol)
[@@deriving sexp]

type func =
  (symbol * typed_symbol list * expr * typ)
[@@deriving sexp]

type prog =
  func list
[@@deriving sexp]

type ast =
  prog
[@@deriving sexp]

module SymbolTable = struct
  include Map.Make(struct
      type t = symbol
      let t_of_sexp = symbol_of_sexp
      let sexp_of_t = sexp_of_symbol
      let compare = compare
    end)
end
