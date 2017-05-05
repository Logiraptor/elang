
type module_ = Loader.inner_module
type program = Llvm.llmodule

let llctx = Llvm.global_context()
let llm = Llvm.create_module llctx "elang.main"
let i32_t = Llvm.i32_type llctx
let i1_t = Llvm.i1_type llctx

exception UndefinedSymbol of string
exception UncompilableExpression of string

let string_of_program = Llvm.string_of_llmodule

let pf =
  let open Llvm in
  let i8_t = i8_type llctx in
  let i32_t = i32_type llctx in
  let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
  let printf = declare_function "printf" printf_ty llm in
  add_function_attr printf Attribute.Nounwind ;
  add_param_attr (param printf 0) Attribute.Nocapture ;
  printf


let add_hello_world llbuilder = 
  let open Llvm in
  let s = build_global_stringptr "Hello, world!\n" "" llbuilder in
  let zero = const_int i32_t 0 in
  let s = build_in_bounds_gep s [| zero |] "" llbuilder in
  let _ = build_call pf [| s |] "" llbuilder in
  ()

let uncompilable expr =
  raise (UncompilableExpression (Ast.sexp_of_expr expr |> Sexplib.Sexp.to_string_hum))

let rec generate_expr names llbuilder expr =
  match expr with
  | Ast.Int i -> (Llvm.const_int i32_t i)
  | Ast.BinOp (op, lhs, rhs) ->
    generate_bin_op names llbuilder (op, lhs, rhs)
  | Ast.Apply (Ast.ID f, args) ->
    let callee =
      (match Llvm.lookup_function f llm with
       | Some callee -> callee
       | None -> raise (UndefinedSymbol (Printf.sprintf "no such function %s" f))) in
    let args = List.map (generate_expr names llbuilder) args in
    Llvm.build_call callee (Array.of_list args) "calltmp" llbuilder

  | Ast.ID i -> 
    (match Ast.SymbolTable.find names i with
     | None -> raise (UndefinedSymbol (Printf.sprintf "no such variable %s" i))
     | Some x -> x)
  | expr -> uncompilable expr
and generate_bin_op names llbuilder (op, lhs, rhs) =
  let lhs = generate_expr names llbuilder lhs in
  let rhs =  generate_expr names llbuilder rhs in
  match op with
  | Ast.Add -> Llvm.build_add lhs rhs "addtmp" llbuilder
  | Ast.Equal -> Llvm.build_icmp Llvm.Icmp.Eq lhs rhs "addtmp" llbuilder

let rec lltype_from_etype (etype : Loader.typ) =
  let open Loader in
  match etype with
  | Arrow (input, output) -> 
    let args = List.map lltype_from_etype input in
    Llvm.function_type i32_t (Array.of_list args)
  | Int -> i32_t
  | Bool -> i1_t


let add_arg_decl names (typ, arg) =
  Ast.SymbolTable.add names arg (Llvm.const_int i32_t 0)

let name_args names f args =
  List.fold_left2 (fun names llparam (_, name) ->
      Llvm.set_value_name name llparam;
      Ast.SymbolTable.add names name llparam 
    ) names (Llvm.params f |> Array.to_list) args

let generate_func names (name, args, expr, _) =
  (*let _ = print_string name; print_newline () in*)
  let Some f = Ast.SymbolTable.find names name in
  let llbuilder = Llvm.builder_at_end llctx (Llvm.entry_block f) in
  let _ = add_hello_world llbuilder in
  let localnames = name_args names f args in
  (*let _ = Ast.SymbolTable.iter (fun name _ -> print_string name; print_newline ()) localnames in*)
  let return_value = generate_expr localnames llbuilder expr in
  let _ = Llvm.build_ret return_value llbuilder in 
  let _ = Llvm_analysis.assert_valid_function f in
  ()

let generate_func_definition types (name, args, _, _)  =
  let etype = Ast.SymbolTable.find_exn types name in
  let ftype = lltype_from_etype etype in
  Llvm.define_function name ftype llm

let add_func_decl types names ((name, args, expr, _) as f) =
  let f = generate_func_definition types f in
  Ast.SymbolTable.add names name f

let generate_code m =
  let open Loader in
  let names = List.fold_left (add_func_decl m.types) Ast.SymbolTable.empty m.ast in
  let _ = List.iter (generate_func names) m.ast in
  let _ = Llvm_analysis.assert_valid_module llm in
  let _ = Llvm.dump_module llm in
  Result.Ok llm
