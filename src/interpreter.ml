open Sexplib.Std
open Core.Std


type value = int [@@deriving sexp]
type program = Compiler.program

let string_of_value v =
  sexp_of_value v
  |> Sexp.to_string_hum

let safe_run (cmd : string) () : (unit, int) Result.t =
  let exit_status = Sys.command cmd in
  match exit_status with
  | 0 -> Result.Ok ()
  | err -> Result.Error err

let result r =
  match r with
  | Ok _ -> Ok 0
  | Error i -> Error (string_of_int i)

let execute prog =
  let open Result in
  let _ = Llvm_bitwriter.write_bitcode_file prog "output.bc" in 
  safe_run "llc-3.8 output.bc" ()
  >>= safe_run "gcc -c output.s" 
  >>= safe_run "gcc -o output output.o"
  >>= safe_run "rm output.*"
  |> result
