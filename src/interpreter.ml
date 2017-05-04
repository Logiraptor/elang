open Sexplib.Std

(*let add_target_triple triple llm =
  Llvm_X86.initialize ();
  let lltarget  = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly     = Llvm_target.TargetMachine.data_layout llmachine in

  Llvm.set_target_triple (Llvm_target.TargetMachine.triple llmachine) llm ;
  Llvm.set_data_layout (Llvm_target.DataLayout.as_string lldly) llm ;
  ()*)


type value = int [@@deriving sexp]
type program = Compiler.program

let execute prog =
  (*let _ = add_target_triple "x86_64" prog in*)
  let _ = Llvm_bitwriter.write_bitcode_file prog "output.bc" in
  Result.Ok 9
