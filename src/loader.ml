open Core.Std
open Lexing
open Printf

type typ = 
  | Int
  | Char
  | Bool
  | String
  | Struct of typed_symbol list
  | Arrow of (typ list * typ)
[@@deriving sexp]

and typed_symbol =
    (typ * Ast.symbol)
[@@deriving sexp]

type symbol = Ast.symbol [@@deriving sexp]

type op = Ast.op [@@deriving sexp]

type ir_expr =
  | IntLit of int
  | ID of symbol
  | StringLit of string
  | StructLit of (typed_symbol list * (symbol * ir_expr) list)
  | FieldAccess of (typed_symbol list * ir_expr * symbol)
  | BinOp of (op * ir_expr * ir_expr)
  | Apply of (ir_expr * ir_expr list)
  | If of (ir_expr * ir_expr * ir_expr)
  | Let of (symbol * ir_expr * ir_expr)
  | TailApply of (ir_expr * ir_expr list)
[@@deriving sexp]

type ir_func =
  (symbol * typed_symbol list * ir_expr * typ)
[@@deriving sexp]

type ir_extern =
  (symbol * typed_symbol list * typ)
[@@deriving sexp]

type ir_typ_decl =
  (symbol * typ)
[@@deriving sexp]

type ir_decl =
  | FuncDecl of ir_func
  | ExternDecl of ir_extern
  | TypeDecl of ir_typ_decl
[@@deriving sexp]

type ir_prog =
  ir_decl list
[@@deriving sexp]

type ir_ast =
  ir_prog
[@@deriving sexp]

type typemap = typ Ast.SymbolTable.t [@@deriving sexp]

type ctx = {
  types : typemap;
  value_types : typemap;
}[@@deriving sexp]

type inner_module = {
  ast : ir_ast;
  ctx : ctx
} [@@deriving sexp]

module Make
    (FR : Language.FileResolver)
    (P : Language.Parser with type ast = Ast.ast and type filetype = FR.filetype) =
struct

  type module_ = inner_module

  exception UntypecheckableExpression of string
  exception Uncallable of string
  exception TypeError of string
  exception LoaderUndefinedSymbol of string

  let built_in_types = Ast.SymbolTable.of_alist_exn [
      ("i1", Bool);
      ("i8", Char);
      ("i32", Int);
      ("str", String)
    ]

  let rec load_type ctx typ =
    match typ with
    | Ast.NamedType s ->
      (match Ast.SymbolTable.find ctx.types s with
       | None -> raise (LoaderUndefinedSymbol s)
       | Some t -> t)
    | Ast.StructType args ->
      let convert_arg (t, name) = (load_type ctx t, name) in
      let new_args = List.map ~f:convert_arg args in
      Struct new_args


  let string_of_type t =
    let sexp = sexp_of_typ t in
    Sexp.to_string_hum sexp

  let string_of_type_list types =
    let strings = List.map ~f:string_of_type types in
    String.concat ~sep:";" strings

  let rec type_check_expr (ctx : ctx) (expr : Ast.expr) : (typ * ir_expr) =
    match expr with
    | Ast.ID id -> 
      (match Ast.SymbolTable.find ctx.value_types id with
       | None -> raise (LoaderUndefinedSymbol id)
       | Some x -> (x, ID id))
    | Ast.Int i -> (Int, IntLit i)
    | Ast.String s -> (String, StringLit s)
    | Ast.StructLit fields ->
      let check_field (name, value) = (type_check_expr ctx value, name) in
      let types_and_values = List.map ~f:check_field fields in
      let extract_type ((t, _), name) = (t, name) in
      let extract_value ((_, v), name) = (name, v) in
      let ts = List.map ~f:extract_type types_and_values in
      let vs = List.map ~f:extract_value types_and_values in
      (Struct ts, StructLit (ts, vs))
    | Ast.FieldAccess (v, field) ->
      (let (stype, sval) = type_check_expr ctx v in
       match stype with
       | Struct fields ->
         (let with_name name (_, candidate) = candidate = name in
          let structField = List.find fields ~f:(with_name field) in
          match structField with 
          | Some (t, _) -> (t, FieldAccess (fields, sval, field))
          | None -> raise (TypeError (Printf.sprintf !"no such field %s in type %{string_of_type}" field stype)))
       | other -> raise (TypeError (Printf.sprintf !"cannot access field of non-struct type: %{string_of_type}" other)))
    | Ast.Apply (f, args) ->
      (let (ftype, fval) = type_check_expr ctx f in
       let types_and_values = List.map ~f:(type_check_expr ctx) args in
       let extract_type (t, _) = t in
       let extract_value (_, v) = v in
       let argtypes = List.map ~f:extract_type types_and_values in
       let argvals = List.map ~f:extract_value types_and_values in
       match ftype with
       | Arrow (input, output) ->
         if input = argtypes then
           (output, Apply (fval, argvals))
         else
           raise (TypeError (Printf.sprintf !"argument type mismatch: expected %{string_of_type_list} got %{string_of_type_list}" input argtypes))
       | t -> raise (Uncallable (sexp_of_typ t |> Sexplib.Sexp.to_string_hum))
      )
    | Ast.BinOp (op, lhs, rhs) ->
      (let (lhstype, lhsval) = type_check_expr ctx lhs in
       let (rhstype, rhsval) = type_check_expr ctx rhs in
       match (op, lhstype, rhstype) with
       | (Ast.Add, Int, Int) -> (Int, BinOp (op, lhsval, rhsval))
       | (Ast.Sub, Int, Int) -> (Int, BinOp (op, lhsval, rhsval))
       | (Ast.Mul, Int, Int) -> (Int, BinOp (op, lhsval, rhsval))
       | (Ast.Equal, Int, Int) -> (Bool, BinOp (op, lhsval, rhsval))
       | (Ast.Mod, Int, Int) -> (Int, BinOp (op, lhsval, rhsval))
       | (Ast.And, Bool, Bool) -> (Bool, BinOp (op, lhsval, rhsval))
       | (otherOp, Int, Int) -> raise (TypeError (Printf.sprintf !"undefined operator %{Sexp#hum}" (Ast.sexp_of_op otherOp)))
       | other -> raise (TypeError (Printf.sprintf !"argument type mismatch: got %{string_of_type_list}" [lhstype; rhstype])))
    | Ast.If (cond, conseq, alt) ->
      (let (condType, condVal) = type_check_expr ctx cond in
       let (conseqType, conseqVal) = type_check_expr ctx conseq in
       let (altType, altVal) = type_check_expr ctx alt in
       if conseqType <> altType then
         raise (TypeError (Printf.sprintf !"branch mismatch error: %{string_of_type} is not compatible with %{string_of_type}" conseqType altType))
       else
       if condType <> Bool then
         raise (TypeError (Printf.sprintf !"condition type: expected Bool got %{string_of_type}" condType))
       else
         (conseqType, If (condVal, conseqVal, altVal))
      )
    | Ast.Let (name, value, body) ->
      let (valType, valVal) = type_check_expr ctx value in
      let localctx = {ctx with value_types = Ast.SymbolTable.add ctx.value_types name valType } in
      let (bodyType, bodyVal) = type_check_expr localctx body in
      (bodyType, Let(name, valVal, bodyVal))
    | expr -> raise (UntypecheckableExpression (Ast.sexp_of_expr expr |> Sexplib.Sexp.to_string_hum))

  let rec identify_tail_calls expr =
    match expr with
    | Let (name, value, body) -> Let (name, value, identify_tail_calls body)
    | If (cond, conseq, alt) -> If (cond, identify_tail_calls conseq, identify_tail_calls alt)
    (*| Apply (f, args) -> TailApply (f, args)*)
    | expr -> expr

  let type_check_arg (ctx : ctx) ((typ, sym) : Ast.typed_symbol) : ctx =
    {ctx with value_types = Ast.SymbolTable.add ctx.value_types sym (load_type ctx typ)}

  let type_check_func (ctx : ctx) ((name, args, body, rtype) : Ast.func) : ir_func =
    let arg_type (t, n) = (load_type ctx t, n) in
    let ir_args = List.map ~f:arg_type args in
    let arg_types = List.map ~f:Tuple2.get1 ir_args in
    let expected_rtype = load_type ctx rtype in
    let expected_ftype = Arrow (arg_types, expected_rtype) in
    let types_with_self = {ctx with value_types = Ast.SymbolTable.add ctx.value_types name expected_ftype} in
    let localctx = List.fold_left ~init:types_with_self ~f:type_check_arg args in
    let (bodyType, bodyVal) = type_check_expr localctx body in
    if bodyType <> expected_rtype then
      raise (TypeError (Printf.sprintf !"return type mismatch: expected %{string_of_type} got %{string_of_type}" expected_rtype bodyType))
    else
      (name, ir_args, bodyVal, bodyType)

  let type_check_extern ctx (name, args, rtype) : ir_extern =
    let arg_type (t, n) = (load_type ctx t, n) in
    let ir_args = List.map ~f:arg_type args in
    let args = List.map ~f:Tuple2.get1 ir_args in
    let expected_rtype = load_type ctx rtype in
    (name, ir_args, expected_rtype)

  let type_check_decl (ctx : ctx) (decl : Ast.decl) : ir_decl =
    match decl with
    | Ast.FuncDecl f -> FuncDecl (type_check_func ctx f)
    | Ast.ExternDecl e -> ExternDecl (type_check_extern ctx e)
    | Ast.TypeDecl (name, t) -> TypeDecl (name, load_type ctx t)

  let transform_decl (f : ir_expr -> ir_expr) (d : ir_decl) =
    match d with
    | FuncDecl (name, args, body, rtype) -> FuncDecl (name, args, f body, rtype)
    | ExternDecl e -> ExternDecl e
    | TypeDecl t -> TypeDecl t

  let add_user_types (ctx : ctx) (d : Ast.decl) : ctx =
    match d with
    | Ast.TypeDecl (name, t) -> {ctx with types=Ast.SymbolTable.add ctx.types name (load_type ctx t)}
    | d -> ctx

  let transform_ir (f : ir_expr -> ir_expr) (m : ir_ast) =
    List.map m (transform_decl f)

  let preload_func (ctx : ctx) ((name, args, body, rtype) : Ast.func) : ctx =
    let arg_type (t, _) = load_type ctx t in
    let expected_rtype = load_type ctx rtype in
    let expected_ftype = Arrow (List.map ~f:arg_type args, expected_rtype) in
    let types_with_self = {ctx with value_types = Ast.SymbolTable.add ctx.value_types name expected_ftype} in
    types_with_self

  let preload_extern (ctx : ctx) (name, args, rtype) =
    let arg_type (t, _) = load_type ctx t in
    let expected_rtype = load_type ctx rtype in
    let expected_ftype = Arrow (List.map ~f:arg_type args, expected_rtype) in
    {ctx with value_types = Ast.SymbolTable.add ctx.value_types name expected_ftype}

  let preload_decl (ctx : ctx) decl : ctx =
    match decl with
    | Ast.FuncDecl f ->
      preload_func ctx f
    | Ast.ExternDecl e ->
      preload_extern ctx e
    | Ast.TypeDecl t -> ctx

  let type_check (m : Ast.ast) =
    let ctx = {value_types=Ast.SymbolTable.empty; types=built_in_types} in
    let user_defined_types = List.fold_left ~init:ctx ~f:add_user_types m in
    let ftypes = List.fold_left ~init:user_defined_types ~f:preload_decl m in
    let ir = List.map ~f:(type_check_decl ftypes) m in
    let out = transform_ir identify_tail_calls ir in
    Ok {ast = out; ctx = ftypes}

  let load_module filename =
    let open Result in
    FR.resolve filename
    >>= P.parse_file
    >>= type_check

  let string_of_module m =
    sexp_of_inner_module m
    |> Sexplib.Sexp.to_string_hum

end
