open OUnit2

let describe (desc : string) (examples : test list) : test =
  desc >::: examples

let it (desc : string) (body : unit -> unit) : test =
  desc >:: (fun ctx -> (body ()))

let eql f expected actual =
  let expectedsexp = f expected in
  let actualsexp = f actual in
  assert_equal ~printer: Sexplib.Sexp.to_string_hum actualsexp expectedsexp

