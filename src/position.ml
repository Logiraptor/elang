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

type 'a with_pos =
  ('a * region)
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
