open Datatypes
open Printf
open Sexplib.Std

module CodeEmitter = struct
  let _defined_modules = ref []

  type module_ = string
  type func = string
  type block = string
  type instruction =
      | ConstInt of int
      [@@deriving sexp]
 
  let output () : string =
      List.fold_left (^) "" (!_defined_modules)

  let reset () =
      _defined_modules := []

  let emit_const_int value b =
      sprintf "%sconst_int %d;" b value

  let open_module name =
      name ^ "{"
  
  let close_module m =
      _defined_modules := (m ^ "}")::(!_defined_modules)
  
  let open_function name m =
      m ^ name ^ "("

  let close_function f =
      f ^ ")"

  let start_block name m =
      m ^ name ^ "["
  
  let end_block b =
      b ^ "]"
end
