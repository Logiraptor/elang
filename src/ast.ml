open Core.Std

type symbol =
  string
[@@deriving sexp]

type op =
  | Add
  | Mul
  | Sub
  | Mod
  | Equal
  | And
[@@deriving sexp]

type expr = 
  | Int of int
  | ID of symbol
  | String of string
  | StructLit of (symbol * expr_with_pos) list
  | FieldAccess of (expr_with_pos * symbol)
  | BinOp of (op * expr_with_pos * expr_with_pos)
  | Apply of (expr_with_pos * expr_with_pos list)
  | If of (expr_with_pos * expr_with_pos * expr_with_pos)
  | Let of (symbol * expr_with_pos * expr_with_pos)
[@@deriving sexp]

and expr_with_pos =
    expr Position.with_pos

type typ =
  | NamedType of symbol
  | StructType of typed_symbol list
  | TypeVariable of symbol
[@@deriving sexp]

and typed_symbol =
    (typ * symbol)
[@@deriving sexp]

type func =
  (symbol * typed_symbol list * expr_with_pos * typ)
[@@deriving sexp]

type extern =
  (symbol * typed_symbol list * typ)
[@@deriving sexp]

type typ_decl =
  (symbol * typ)
[@@deriving sexp]

type decl =
  | FuncDecl of func
  | ExternDecl of extern
  | TypeDecl of typ_decl
[@@deriving sexp]

type prog =
  decl list
[@@deriving sexp]

type ast =
  prog
[@@deriving sexp]

exception Error of string Position.with_pos [@@deriving sexp]

module SymbolTable = struct
  include Map.Make(struct
      type t = symbol
      let t_of_sexp = symbol_of_sexp
      let sexp_of_t = sexp_of_symbol
      let compare = compare
    end)
end
