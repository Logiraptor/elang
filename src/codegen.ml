open Datatypes
open Sexplib.Std

module Make(Emitter : Sigs.CodeEmitter) = struct
    type typed_ast = 
        | Int of int
        | String of string
        [@@deriving sexp]

    let generate_expr (Int x) fn =
        fn
        |> Emitter.start_block "expr"
        |> Emitter.emit_const_int x
        |> Emitter.end_block
end
