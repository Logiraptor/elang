open Spec
open OUnit2
open Fakes

let suite = describe "FakeVM" [
    it "can push and pop values from the stack" (fun () ->
        FakeVM.push_stack (FakeVM.new_int 0);
        assert_equal (FakeVM.pop_stack ()) (Some (FakeVM.new_int 0))
    )
]
