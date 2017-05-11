
type module_ = Loader.inner_module
type program = Llvm.llmodule

let llctx = Llvm.global_context()
let llm = Llvm.create_module llctx "elang.main"
let i32_t = Llvm.i32_type llctx
let i1_t = Llvm.i1_type llctx

let i8_t = Llvm.i8_type llctx
let str_t = Llvm.pointer_type i8_t

exception UndefinedSymbol of string
exception UncompilableExpression of string

let string_of_program = Llvm.string_of_llmodule
(*
let pf =
  let open Llvm in
  let i8_t = i8_type llctx in
  let i32_t = i32_type llctx in
  let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
  let printf = declare_function "printf" printf_ty llm in
  add_function_attr printf Attribute.Nounwind ;
  add_param_attr (param printf 0) Attribute.Nocapture ;
  printf*)


(*let add_hello_world llbuilder = 
  let open Llvm in
  let s = build_global_stringptr "Hello, world!\n" "" llbuilder in
  let zero = const_int i32_t 0 in
  let s = build_in_bounds_gep s [| zero |] "" llbuilder in
  let _ = build_call pf [| s |] "" llbuilder in
  ()*)

let rec size_of t =
  let open Typed_ast in
  match t with
  | Int n -> n
  | String -> 1

let rec get_field_offset structType field start =
  match structType with
  | [] -> raise (UncompilableExpression (Printf.sprintf !"no such field %s in struct" field))
  | (t, name)::rest ->
    if name = field then
      start
    else
      get_field_offset rest field (start + (size_of t))

let uncompilable expr =
  raise (UncompilableExpression (Ast.sexp_of_expr expr |> Sexplib.Sexp.to_string_hum))

let rec generate_expr names llbuilder expr =
  match expr with
  | Ast.Int i -> (Llvm.const_int i32_t i)
  | Ast.String s -> Llvm.build_global_stringptr s "" llbuilder
  | Ast.BinOp (op, lhs, rhs) ->
    generate_bin_op names llbuilder (op, lhs, rhs)
  | Ast.Apply (Ast.ID f, args) ->
    generate_call names llbuilder (Ast.ID f) args
  | Ast.TailApply (Ast.ID f, args) ->
    let call = generate_call names llbuilder (Ast.ID f) args in
    let _ = Llvm.set_tail_call true call in
    call
  | Ast.ID i -> 
    (match Ast.SymbolTable.find names i with
     | None -> raise (UndefinedSymbol (Printf.sprintf "no such variable %s" i))
     | Some x -> x)
  | Ast.If (cond, conseq, alt) -> generate_branch names llbuilder (cond, conseq, alt)
  | Ast.Let (name, value, body) ->
    let value = generate_expr names llbuilder value in
    let localNames = Ast.SymbolTable.add names name value in
    generate_expr localNames llbuilder body
  | Ast.FieldAccess (receiver, field) ->
    let value = generate_expr names llbuilder receiver in
    let offset = get_field_offset receiver field in
    Llvm.build_in_bounds_gep value offset "indextmp" llbuilder
  | expr -> uncompilable expr
and generate_call names llbuilder (Ast.ID f) args =
  let callee =
    (match Llvm.lookup_function f llm with
     | Some callee -> callee
     | None -> raise (UndefinedSymbol (Printf.sprintf "no such function %s" f))) in
  let args = List.map (generate_expr names llbuilder) args in
  Llvm.build_call callee (Array.of_list args) "calltmp" llbuilder
and generate_bin_op names llbuilder (op, lhs, rhs) =
  let lhs = generate_expr names llbuilder lhs in
  let rhs =  generate_expr names llbuilder rhs in
  match op with
  | Ast.Add -> Llvm.build_add lhs rhs "addtmp" llbuilder
  | Ast.Mul -> Llvm.build_mul lhs rhs "multmp" llbuilder
  | Ast.Sub -> Llvm.build_sub lhs rhs "subtmp" llbuilder
  | Ast.Mod -> Llvm.build_srem lhs rhs "modtmp" llbuilder
  | Ast.Equal -> Llvm.build_icmp Llvm.Icmp.Eq lhs rhs "eqltmp" llbuilder
  | Ast.And -> Llvm.build_and lhs rhs "andtmp" llbuilder
and generate_branch names llbuilder (cond, conseq, alt) =
  let cond = generate_expr names llbuilder cond in
  let start_bb = Llvm.insertion_block llbuilder in
  let the_function = Llvm.block_parent start_bb in
  let then_bb = Llvm.append_block llctx "conseq" the_function in
  Llvm.position_at_end then_bb llbuilder;
  let conseq = generate_expr names llbuilder conseq in
  let new_then_bb = Llvm.insertion_block llbuilder in
  (* Emit 'else' value. *)
  let else_bb = Llvm.append_block llctx "else" the_function in
  Llvm.position_at_end else_bb llbuilder;
  let else_val = generate_expr names llbuilder alt in

  (* Codegen of 'else' can change the current block, update else_bb for the
   * phi. *)
  let new_else_bb = Llvm.insertion_block llbuilder in

  (* Emit merge block. *)
  let merge_bb = Llvm.append_block llctx "ifcont" the_function in
  Llvm.position_at_end merge_bb llbuilder;
  let incoming = [(conseq, new_then_bb); (else_val, new_else_bb)] in
  let phi = Llvm.build_phi incoming "iftmp" llbuilder in

  (* Return to the start block to add the conditional branch. *)
  Llvm.position_at_end start_bb llbuilder;
  ignore (Llvm.build_cond_br cond then_bb else_bb llbuilder);

  (* Set a unconditional branch at the end of the 'then' block and the
   * 'else' block to the 'merge' block. *)
  Llvm.position_at_end new_then_bb llbuilder; ignore (Llvm.build_br merge_bb llbuilder);
  Llvm.position_at_end new_else_bb llbuilder; ignore (Llvm.build_br merge_bb llbuilder);

  (* Finally, set the llbuilder to the end of the merge block. *)
  Llvm.position_at_end merge_bb llbuilder;
  phi

let rec lltype_from_etype (etype : Loader.typ) =
  let open Loader in
  match etype with
  | Arrow (input, output) -> 
    let args = List.map lltype_from_etype input in
    Llvm.function_type (lltype_from_etype output) (Array.of_list args)
  | Int -> i32_t
  | Char -> i8_t
  | Bool -> i1_t
  | String -> str_t
  | Struct fields ->
    let typ_of (t, _) = lltype_from_etype t in
    let arg_types = List.map typ_of fields in
    Llvm.struct_type llctx (Array.of_list arg_types)

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
  (*let _ = add_hello_world llbuilder in*)
  let localnames = name_args names f args in
  (*let _ = Ast.SymbolTable.iter (fun name _ -> print_string name; print_newline ()) localnames in*)
  let return_value = generate_expr localnames llbuilder expr in
  let _ = Llvm.build_ret return_value llbuilder in 
  let _ = Llvm_analysis.assert_valid_function f in
  ()

let generate_decl names decl =
  match decl with
  | Ast.FuncDecl f -> generate_func names f
  | Ast.ExternDecl e -> ()
  | Ast.TypeDecl t -> ()

let ast_func_to_lltype types name args =
  let Some etype = Ast.SymbolTable.find types name in
  lltype_from_etype etype

let add_func_decl types names (name, args, expr, _) =
  let ftype = ast_func_to_lltype types name args in
  let f = Llvm.define_function name ftype llm in
  Ast.SymbolTable.add names name f

let add_extern_decl types names (name, args, _) =
  let ftype = ast_func_to_lltype types name args in
  let f = Llvm.declare_function name ftype llm in
  Ast.SymbolTable.add names name f

let add_decl types names decl =
  match decl with
  | Ast.FuncDecl f -> add_func_decl types names f
  | Ast.ExternDecl e -> add_extern_decl types names e
  | Ast.TypeDecl e -> names

let generate_code m =
  let open Loader in
  let names = List.fold_left (add_decl m.ctx.value_types) Ast.SymbolTable.empty m.ast in
  let _ = List.iter (generate_decl names) m.ast in
  let _ = Llvm_analysis.assert_valid_module llm in
  let _ = Llvm.dump_module llm in
  Result.Ok llm
