open Core.Std
open Lexing
open Printf

type typ = 
  | Int
  | Bool
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
    | "int" -> Int
    | "bool" -> Bool
    | _ -> raise (UndefinedSymbol s)

  let string_of_type_list types =
    let sexps = List.map ~f:sexp_of_typ types in
    let strings = List.map ~f:Sexp.to_string_hum sexps in
    String.concat ~sep:";" strings

  let rec type_check_expr (types : typemap) (expr : Ast.expr) : typ =
    match expr with
    | Ast.ID id -> 
      (match Ast.SymbolTable.find types id with
       | None -> raise (UndefinedSymbol id)
       | Some x -> x)
    | Ast.Int _ -> Int
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
       | (Ast.Equal, Int, Int) -> Bool
       | other -> raise (TypeError (Printf.sprintf !"argument type mismatch: expected (Int, Int) got %{string_of_type_list}" [lhstype; rhstype])))
    | expr -> raise (UntypecheckableExpression (Ast.sexp_of_expr expr |> Sexplib.Sexp.to_string_hum))

  let type_check_arg (types : typemap) ((typ, sym) : Ast.typed_symbol) : typemap =
    Ast.SymbolTable.add types sym (convert_type_id typ)

  let type_check_funcs (types : typemap) ((name, args, body, rtype) : Ast.func) : typemap =
    let localtypes = List.fold_left ~init:types ~f:type_check_arg args in
    let body_type = type_check_expr localtypes body in
    Ast.SymbolTable.add types name (Arrow (List.init (List.length args) (fun _ -> Int), (convert_type_id rtype)))

  let type_check (m : Ast.ast) =
    let ftypes = List.fold_left ~init:Ast.SymbolTable.empty ~f:type_check_funcs m in
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
