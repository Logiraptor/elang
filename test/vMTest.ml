open Spec
open OUnit2
open Fakes

module Make(Cut : Sigs.VM) = struct
    let suite = describe "VM" [
        it "can push and pop values from the stack" (fun () ->
            Cut.push_stack (Cut.new_int 0);
            assert_equal (Cut.pop_stack ()) (Some (Cut.new_int 0))
        )
    ]
end

