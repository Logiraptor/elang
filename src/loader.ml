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

type typemap = typ Ast.SymbolTable.t [@@deriving sexp]

type ctx = {
  types : typemap;
  value_types : typemap;
}[@@deriving sexp]

type inner_module = {
  ast : Ast.ast;
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

  let rec type_check_expr (ctx : ctx) (expr : Ast.expr) : typ =
    match expr with
    | Ast.ID id -> 
      (match Ast.SymbolTable.find ctx.value_types id with
       | None -> raise (LoaderUndefinedSymbol id)
       | Some x -> x)
    | Ast.Int _ -> Int
    | Ast.String _ -> String
    | Ast.FieldAccess (v, field) ->
      (let stype = type_check_expr ctx v in
       match stype with
       | Struct fields ->
         (let with_name name (_, candidate) = candidate = name in
          let structField = List.find fields ~f:(with_name field) in
          match structField with 
          | Some (t, _) -> t
          | None -> raise (TypeError (Printf.sprintf !"no such field %s in type %{string_of_type}" field stype)))
       | other -> raise (TypeError (Printf.sprintf !"cannot access field of non-struct type: %{string_of_type}" other)))
    | Ast.Apply (f, args) ->
      (let ftype = type_check_expr ctx f in
       let argtypes = List.map ~f:(type_check_expr ctx) args in
       match ftype with
       | Arrow (input, output) ->
         if input = argtypes then
           output
         else
           raise (TypeError (Printf.sprintf !"argument type mismatch: expected %{string_of_type_list} got %{string_of_type_list}" input argtypes))
       | t -> raise (Uncallable (sexp_of_typ t |> Sexplib.Sexp.to_string_hum))
      )
    | Ast.BinOp (op, lhs, rhs) ->
      (let lhstype = type_check_expr ctx lhs in
       let rhstype = type_check_expr ctx rhs in
       match (op, lhstype, rhstype) with
       | (Ast.Add, Int, Int) -> Int
       | (Ast.Sub, Int, Int) -> Int
       | (Ast.Mul, Int, Int) -> Int
       | (Ast.Equal, Int, Int) -> Bool
       | (Ast.Mod, Int, Int) -> Int
       | (Ast.And, Bool, Bool) -> Bool
       | (otherOp, Int, Int) -> raise (TypeError (Printf.sprintf !"undefined operator %{Sexp#hum}" (Ast.sexp_of_op otherOp)))
       | other -> raise (TypeError (Printf.sprintf !"argument type mismatch: got %{string_of_type_list}" [lhstype; rhstype])))
    | Ast.If (cond, conseq, alt) ->
      (let condType = type_check_expr ctx cond in
       let conseqType = type_check_expr ctx conseq in
       let altType = type_check_expr ctx alt in
       if conseqType <> altType then
         raise (TypeError (Printf.sprintf !"branch mismatch error: %{string_of_type} is not compatible with %{string_of_type}" conseqType altType))
       else
       if condType <> Bool then
         raise (TypeError (Printf.sprintf !"condition type: expected Bool got %{string_of_type}" condType))
       else
         conseqType
      )
    | Ast.Let (name, value, body) ->
      (let valType = type_check_expr ctx value in
       let localctx = {ctx with value_types = Ast.SymbolTable.add ctx.value_types name valType } in
       type_check_expr localctx body)
    | expr -> raise (UntypecheckableExpression (Ast.sexp_of_expr expr |> Sexplib.Sexp.to_string_hum))

  let rec identify_tail_calls expr =
    match expr with
    | Ast.Let (name, value, body) -> Ast.Let (name, value, identify_tail_calls body)
    | Ast.If (cond, conseq, alt) -> Ast.If (cond, identify_tail_calls conseq, identify_tail_calls alt)
    | Ast.Apply (f, args) -> Ast.TailApply (f, args)
    | expr -> expr

  let type_check_arg (ctx : ctx) ((typ, sym) : Ast.typed_symbol) : ctx =
    {ctx with value_types = Ast.SymbolTable.add ctx.value_types sym (load_type ctx typ)}

  let type_check_funcs (ctx : ctx) ((name, args, body, rtype) : Ast.func) : ctx =
    let arg_type (t, _) = load_type ctx t in
    let expected_rtype = load_type ctx rtype in
    let expected_ftype = Arrow (List.map ~f:arg_type args, expected_rtype) in
    let types_with_self = {ctx with value_types = Ast.SymbolTable.add ctx.value_types name expected_ftype} in
    let localctx = List.fold_left ~init:types_with_self ~f:type_check_arg args in
    let body_type = type_check_expr localctx body in
    if body_type <> expected_rtype then
      raise (TypeError (Printf.sprintf !"return type mismatch: expected %{string_of_type} got %{string_of_type}" expected_rtype body_type))
    else
      {ctx with value_types = Ast.SymbolTable.add ctx.value_types name expected_ftype }

  let type_check_extern ctx (name, args, rtype) =
    let arg_type (t, _) = load_type ctx t in
    let expected_rtype = load_type ctx rtype in
    let expected_ftype = Arrow (List.map ~f:arg_type args, expected_rtype) in
    {ctx with value_types = Ast.SymbolTable.add ctx.value_types name expected_ftype}

  let type_check_decl (ctx : ctx) decl : ctx =
    match decl with
    | Ast.FuncDecl f -> type_check_funcs ctx f
    | Ast.ExternDecl e -> type_check_extern ctx e
    | Ast.TypeDecl t -> ctx

  let transform_decl (f : Ast.expr -> Ast.expr) (d : Ast.decl) =
    match d with
    | Ast.FuncDecl (name, args, body, rtype) -> Ast.FuncDecl (name, args, f body, rtype)
    | Ast.ExternDecl e -> Ast.ExternDecl e
    | Ast.TypeDecl t -> Ast.TypeDecl t

  let add_user_types (ctx : ctx) (d : Ast.decl) : ctx =
    match d with
    | Ast.TypeDecl (name, t) -> {ctx with types=Ast.SymbolTable.add ctx.types name (load_type ctx t)}
    | d -> ctx

  let transform_ast (f : Ast.expr -> Ast.expr) (m : Ast.ast) =
    List.map m (transform_decl f)

  let type_check (m : Ast.ast) =
    let ctx = {value_types=Ast.SymbolTable.empty; types=built_in_types} in
    let user_defined_types = List.fold_left ~init:ctx ~f:add_user_types m in
    let ftypes = List.fold_left ~init:user_defined_types ~f:type_check_decl m in
    let out = transform_ast identify_tail_calls m in
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
