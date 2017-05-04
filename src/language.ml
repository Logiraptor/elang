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
end

(*Compiler takes a module and turns it into a program*)
module type Compiler = sig
  type module_
  type program
  val generate_code : module_ -> (program, string) Result.t
end

(*Interpreter executes a program and results in a value*)
module type Interpreter = sig
  type program
  type value [@@deriving sexp]
  val execute : program -> (value, string) Result.t
end

module type Language = sig
  type value [@@deriving sexp]
  val execute : string -> (value, string) Result.t
  val string_of_value : value -> string
end

module Make
    (Loader : Loader)
    (Compile : Compiler with type module_ = Loader.module_) 
    (Interp : Interpreter with type program = Compile.program) : Language =
struct

  let (=>) r f =
    match r with
    | Ok v -> f v
    | Error err -> Error err

  type value = Interp.value [@@deriving sexp]

  let execute filename = 
    let open Result in
    Loader.load_module filename
    >>= Compile.generate_code
    >>= Interp.execute

  let string_of_value v =
    sexp_of_value v |> Sexplib.Sexp.to_string_hum
end
