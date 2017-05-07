open Core.Std
open Lexing
open Printf

type typ = 
  | Int
  | Bool
  | String
  | Arrow of (typ list * typ)
[@@deriving sexp]

type inner_module = {
  ast : Ast.ast;
  types : typ Ast.SymbolTable.t
} [@@deriving sexp]

module Make
    (FR : Language.FileResolver)
    (P : Language.Parser with type ast = Ast.ast and type filetype = FR.filetype) = 
struct

  type module_ = inner_module
  type typemap = typ Ast.SymbolTable.t
  exception UntypecheckableExpression of string
  exception Uncallable of string
  exception TypeError of string
  exception UndefinedSymbol of string

  let convert_type_id (Ast.NamedType s) =
    match s with
    | "i32" -> Int
    | "bool" -> Bool
    | "str" -> String
    | _ -> raise (UndefinedSymbol s)

  let string_of_type t =
    let sexp = sexp_of_typ t in
    Sexp.to_string_hum sexp

  let string_of_type_list types =
    let strings = List.map ~f:string_of_type types in
    String.concat ~sep:";" strings

  let rec type_check_expr (types : typemap) (expr : Ast.expr) : typ =
    match expr with
    | Ast.ID id -> 
      (match Ast.SymbolTable.find types id with
       | None -> raise (UndefinedSymbol id)
       | Some x -> x)
    | Ast.Int _ -> Int
    | Ast.String _ -> String
    | Ast.Apply (f, args) ->
      (let ftype = type_check_expr types f in
       let argtypes = List.map ~f:(type_check_expr types) args in
       match ftype with
       | Arrow (input, output) ->
         if input = argtypes then
           output
         else
           raise (TypeError (Printf.sprintf !"argument type mismatch: expected %{string_of_type_list} got %{string_of_type_list}" input argtypes))
       | t -> raise (Uncallable (sexp_of_typ t |> Sexplib.Sexp.to_string_hum))
      )
    | Ast.BinOp (op, lhs, rhs) ->
      (let lhstype = type_check_expr types lhs in
       let rhstype = type_check_expr types rhs in
       match (op, lhstype, rhstype) with
       | (Ast.Add, Int, Int) -> Int
       | (Ast.Sub, Int, Int) -> Int
       | (Ast.Mul, Int, Int) -> Int
       | (Ast.Equal, Int, Int) -> Bool
       | (otherOp, Int, Int) -> raise (TypeError (Printf.sprintf !"undefined operator %{Sexp#hum}" (Ast.sexp_of_op otherOp)))
       | other -> raise (TypeError (Printf.sprintf !"argument type mismatch: expected (Int, Int) got %{string_of_type_list}" [lhstype; rhstype])))
    | Ast.If (cond, conseq, alt) ->
      (let condType = type_check_expr types cond in
       let conseqType = type_check_expr types conseq in
       let altType = type_check_expr types alt in
       if conseqType <> altType then
         raise (TypeError (Printf.sprintf !"branch mismatch error: %{string_of_type} is not compatible with %{string_of_type}" conseqType altType))
       else
       if condType <> Bool then
         raise (TypeError (Printf.sprintf !"condition type: expected Bool got %{string_of_type}" condType))
       else
         conseqType
      )
    | expr -> raise (UntypecheckableExpression (Ast.sexp_of_expr expr |> Sexplib.Sexp.to_string_hum))

  let type_check_arg (types : typemap) ((typ, sym) : Ast.typed_symbol) : typemap =
    Ast.SymbolTable.add types sym (convert_type_id typ)

  let type_check_funcs (types : typemap) ((name, args, body, rtype) : Ast.func) : typemap =
    let arg_type (t, _) = convert_type_id t in
    let expected_rtype = convert_type_id rtype in
    let expected_ftype = Arrow (List.map ~f:arg_type args, expected_rtype) in
    let types_with_self = Ast.SymbolTable.add types name expected_ftype in
    let localtypes = List.fold_left ~init:types_with_self ~f:type_check_arg args in
    let body_type = type_check_expr localtypes body in
    if body_type <> expected_rtype then
      raise (TypeError (Printf.sprintf !"return type mismatch: expected %{string_of_type} got %{string_of_type}" expected_rtype body_type))
    else
      Ast.SymbolTable.add types name expected_ftype

  let type_check_extern types (name, args, rtype) =
    let arg_type (t, _) = convert_type_id t in
    let expected_rtype = convert_type_id rtype in
    let expected_ftype = Arrow (List.map ~f:arg_type args, expected_rtype) in
    Ast.SymbolTable.add types name expected_ftype

  let type_check_decl types decl =
    match decl with
    | Ast.FuncDecl f -> type_check_funcs types f
    | Ast.ExternDecl e -> type_check_extern types e

  let type_check (m : Ast.ast) =
    let ftypes = List.fold_left ~init:Ast.SymbolTable.empty ~f:type_check_decl m in
    Ok {ast = m; types = ftypes}

  let load_module filename =
    let open Result in
    FR.resolve filename
    >>= P.parse_file
    >>= type_check

  let string_of_module m =
    sexp_of_inner_module m
    |> Sexplib.Sexp.to_string_hum

end
