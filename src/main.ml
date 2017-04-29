
module Parser = Parser.Make(SymbolTable)
module Codegen = Codegen.Make(Emitter)
module Interp = Interp.Make(VM.VM)

let () =
    let ast = Parser.parse_file "main.el" in
    let code = Codegen.generate_module ast in
    let result = Interp.execute_module code in
    result
