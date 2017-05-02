
type module_ = Loader.inner_module
type program = int


let generate_func (name, args, expr) =
  match expr with
  | Ast.INT i -> i

let generate_code m =
  let open Loader in
  match m.ast with
  | [] -> Result.Error "You must specify at least one function"
  | h::_ -> Result.Ok (generate_func h)
