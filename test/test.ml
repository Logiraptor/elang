
module type TestSuite = sig
  val suite : OUnit2.test
end

let run (module M : TestSuite) =
  OUnit2.run_test_tt_main M.suite

let () = ()
