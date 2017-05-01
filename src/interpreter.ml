open Sexplib.Std

type value = int [@@deriving sexp]
type program = int

let execute prog = Result.Ok prog
