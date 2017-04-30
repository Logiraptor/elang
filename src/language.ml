

module type Parser = sig
  type ast
  val parse_string : string -> ast
end

module type Loader = sig
  type ast
  type module_
  val load_ast : ast -> module_
end

module type Compiler = sig
  type module_
  type program
  val generate_code : module_ -> program
end

module type Interpreter = sig
  type program
  type value
  val execute : program -> value
end

