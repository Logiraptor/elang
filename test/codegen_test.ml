open OUnit2
open Datatypes
open Printf
open Spec

module Cut = Codegen.Make(Mocks.CodeEmitter)

let assert_modules_equal = assert_equal ~printer: (fun x -> x)

let suite = describe "Codegen" [
  it "generates const_int instructions for ints" (fun () ->
    QCheck.Test.make QCheck.int (fun i ->
      let expected = sprintf "expr[const_int %d;]" i in
      let actual = Cut.generate_expr (Int i) "" in
      expected = actual
    )
    |> QCheck.Test.check_exn
  )
]
