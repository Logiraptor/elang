open Spec
open Datatypes
open Fakes
open Sexplib.Std
open VM

module Cut = Interp.Make(VM)

type stacktype = VM.value list [@@deriving sexp]

let suite = describe "Interp" [
  it "can interpret a constant 0" (fun () ->
    Cut.execute_instruction (ConstInt 0);
    eql sexp_of_stacktype (VM.stack ()) [VM.new_int 0]
  );

  it "can interpret a constant 1" (fun () ->
    Cut.execute_instruction (ConstInt 1);
    eql sexp_of_stacktype (VM.stack ()) [VM.new_int 1]
  )
]
