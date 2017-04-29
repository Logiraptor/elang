
module type Language = sig
    type symbol
    type ast
    type module_
    type program
    type value

    module Parser = sig
        val parse_string : string -> ast
    end

    module Loader = sig
        val load_ast : ast -> module_
    end

    module Compiler = sig
        val generate_code : module_ -> program
    end

    module Interpreter = sig
        val execute : program -> value
    end
end
