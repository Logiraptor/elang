open Core.Std

(*FileResolver resolves a filename to an open file*)
module type FileResolver = sig
  type filetype
  val resolve : string -> (filetype, string) Result.t
end

(*Parser takes a filename and turns it into an ast*)
module type Parser = sig
  type filetype
  type ast
  val parse_file : filetype -> (ast, string) Result.t
end

(*Loader takes a filename and turns it into a module*)
module type Loader = sig
  type module_
  val load_module : string -> (module_, string) Result.t
  val string_of_module : module_ -> string
end

(*Compiler takes a module and turns it into a program*)
module type Compiler = sig
  type module_
  type program
  val generate_code : module_ -> (program, string) Result.t
  val string_of_program : program -> string
end

(*Interpreter executes a program and results in a value*)
module type Interpreter = sig
  type program
  type value
  val execute : string -> program -> (value, string) Result.t
  val string_of_value : value -> string
end

module StringMap = Map.Make(String)

module type Language = sig
  type value
  val execute : bool StringMap.t -> string -> (value, string) Result.t
  val string_of_value : value -> string
end

module Make
    (Loader : Loader)
    (Compile : Compiler with type module_ = Loader.module_) 
    (Interp : Interpreter with type program = Compile.program) : Language =
struct

  type value = Interp.value
  type module_ = Compile.module_
  type program = Interp.program

  let exec stage stringer print filename =
    let m = stage filename in
    Result.map (Result.map m stringer) print_string;
    print_newline ();
    m

  let dump shouldPrint toString v =
    if shouldPrint then
      (toString v |> print_string; print_newline (); Ok v)
    else Ok v

  let execute options filename = 
    let print_module = StringMap.find options "print_module" |> Option.value ~default:false in
    let print_program = StringMap.find options "print_program" |> Option.value ~default:false in
    let open Result in
    Loader.load_module filename
    >>= dump print_module Loader.string_of_module
    >>= Compile.generate_code
    >>= dump print_program Compile.string_of_program
    >>= Interp.execute filename

  let string_of_value =
    Interp.string_of_value

  let string_of_program =
    Compile.string_of_program

  let string_of_module =
    Loader.string_of_module
end
