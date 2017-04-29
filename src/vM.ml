open Datatypes
open Sexplib.Std

module VM = struct
    type value =
        | Int of int [@@deriving sexp]

    let _stack : value list ref = ref []

    let stack () : value list = !_stack


    let push_stack (v : value) : unit =
        _stack := v::!_stack

    let pop_stack () : value option =
        let s = stack() in
        match s with
         | [] -> None
         | h::t -> _stack := t; Some h

    let new_int x =
        Int x
end
