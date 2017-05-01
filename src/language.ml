open Result

(*FileResolver resolves a filename to an open file*)
module type FileResolver = sig
  val resolve : string -> Core.Std.In_channel.t
end

(*Parser takes a filename and turns it into an ast*)
module type Parser = sig
  type ast
  val parse_string : Core.Std.In_channel.t -> ast
end

(*Loader takes a filename and turns it into a module*)
module type Loader = sig
  type module_
  val load_module : string -> (module_, string) result
end

(*Compiler takes a module and turns it into a program*)
module type Compiler = sig
  type module_
  type program
  val generate_code : module_ -> (program, string) result
end

(*Interpreter executes a program and results in a value*)
module type Interpreter = sig
  type program
  type value [@@deriving sexp]
  val execute : program -> (value, string) result
end

module type Language = sig
  type value [@@deriving sexp]
  val execute : string -> (value, string) result
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
    Loader.load_module filename
    => Compile.generate_code
    => Interp.execute

  let string_of_value v =
    sexp_of_value v |> Sexplib.Sexp.to_string_hum
end
