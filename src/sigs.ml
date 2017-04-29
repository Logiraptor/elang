
module type CodeEmitter = sig
  type module_
  type func
  type block

  val open_module : string -> module_
  val close_module : module_ -> unit

  val open_function : string -> module_ -> func
  val close_function : func -> module_

  val start_block : string -> func -> block
  val end_block : block -> func

  val emit_const_int : int -> block -> block
end

module type VM = sig
  type value [@@deriving sexp]

  val new_int : int -> value

  val push_stack : value -> unit
  val pop_stack : unit -> value option
end

module type Interpreter = sig
  type instruction [@@deriving sexp]

  val execute_instruction : instruction -> unit
end
