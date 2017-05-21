open Sexplib.Std
open Core.Std


type value = int [@@deriving sexp]
type program = Codegen.program

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

let execute filename prog =
  let open Result in
  let basename = Filename.basename filename in
  let (progname, _) = Filename.split_extension basename in
  let _ = Llvm_bitwriter.write_bitcode_file prog (progname ^ ".bc") in 
  safe_run (sprintf "llc-3.8 %s.bc" progname) ()
  >>= safe_run (sprintf "gcc -c %s.s" progname) 
  >>= safe_run (sprintf "gcc -o %s %s.o" progname progname)
  >>= safe_run (sprintf "rm %s.*" progname)
  |> result
