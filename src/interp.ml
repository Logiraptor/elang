open Datatypes
open Value
open Sexplib.Std

module Make(VM : Sigs.VM) = struct
  type instruction =
    | ConstInt of int
  [@@deriving sexp]

  let execute_instruction (ConstInt x) =
    VM.push_stack (VM.new_int x)
end
