
type module_ = Loader.inner_module
type program = Llvm.llmodule

let llctx = Llvm.global_context()
let llm = Llvm.create_module llctx "elang.main"
let i32_t = Llvm.i32_type llctx

exception UndefinedSymbol of string

let rec generate_expr names llbuilder expr =
  match expr with
  | Ast.Int i -> (Llvm.const_int i32_t i)
  | Ast.BinOp (Ast.Add, lhs, rhs) -> 
    let lhs = generate_expr names llbuilder lhs in
    let rhs =  generate_expr names llbuilder rhs in
    Llvm.build_add lhs rhs "addtmp" llbuilder
  | Ast.Apply (Ast.ID f, args) ->
    let callee =
      (match Llvm.lookup_function f llm with
       | Some callee -> callee
       | None -> raise (UndefinedSymbol (Printf.sprintf "no such function %s" f))) in
    let args = List.map (generate_expr names llbuilder) args in
    Llvm.build_call callee (Array.of_list args) "calltmp" llbuilder

  | Ast.ID i -> 
    (try Ast.SymbolTable.find i names
     with
     | Not_found -> raise (UndefinedSymbol (Printf.sprintf "no such variable %s" i)))
  | _ -> (Llvm.const_int i32_t 11)

let function_type_from_args args =
  let argtypelist = List.map (fun _ -> i32_t) args in
  let argtypes = Array.of_list argtypelist in
  Llvm.function_type i32_t argtypes

let add_arg_decl names (typ, arg) =
  Ast.SymbolTable.add arg (Llvm.const_int i32_t 0) names

let name_args names f args =
  List.fold_left2 (fun names llparam (_, name) ->
      Llvm.set_value_name name llparam;
      Ast.SymbolTable.add name llparam names
    ) names (Llvm.params f |> Array.to_list)  args

let generate_func names (name, args, expr) =
  (*let _ = print_string name; print_newline () in*)
  let f = Ast.SymbolTable.find name names in
  let llbuilder = Llvm.builder_at_end llctx (Llvm.entry_block f) in
  let localnames = name_args names f args in
  (*let _ = Ast.SymbolTable.iter (fun name _ -> print_string name; print_newline ()) localnames in*)
  let return_value = generate_expr localnames llbuilder expr in
  let _ = Llvm.build_ret return_value llbuilder in 
  let _ = Llvm_analysis.assert_valid_function f in
  ()

let generate_func_definition (name, args, _)  =
  let ftype = function_type_from_args args in
  Llvm.define_function name ftype llm

let add_func_decl names ((name, args, expr) as f) =
  let f = generate_func_definition f in
  Ast.SymbolTable.add name f names

let generate_code m =
  let open Loader in
  let names = List.fold_left add_func_decl Ast.SymbolTable.empty m.ast in
  let _ = List.iter (generate_func names) m.ast in
  let _ = Llvm_analysis.assert_valid_module llm in
  Result.Ok llm
