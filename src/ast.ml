open Core.Std


type position = {
  filename : string;
  linenum : int;
  beginning_of_line : int;
  beginning_of_token : int;
}
[@@deriving sexp]

type region = {
  startPos: position;
  endPos: position
}
[@@deriving sexp]

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

type 'a with_pos =
  ('a * region)
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
    expr with_pos

type typ =
  | NamedType of symbol
  | StructType of typed_symbol list
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

let to_pos (x : Lexing.position) =
  let open Lexing in
  {
    filename = x.pos_fname;
    linenum = x.pos_lnum;
    beginning_of_line = x.pos_bol;
    beginning_of_token = x.pos_cnum
  }

let make_region s e =
  {startPos=to_pos s; endPos=to_pos e}

let capture_pos (pos : region) (expr : 'a) : 'a with_pos =
  (expr, pos)

exception Error of string with_pos [@@deriving sexp]

module SymbolTable = struct
  include Map.Make(struct
      type t = symbol
      let t_of_sexp = symbol_of_sexp
      let sexp_of_t = sexp_of_symbol
      let compare = compare
    end)
end
