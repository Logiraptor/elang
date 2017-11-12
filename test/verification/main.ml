open Core.Std
open Lexing
open Verification
open Core_extended


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf : Ast.ast option =
  try Some (Verify_parser.prog Verify_lexer.read lexbuf) with
  | Verify_lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Verify_parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    printf "%s\n" (Sexp.to_string_hum (Ast.sexp_of_ast value))
  | None -> ()

let load_file (filename: string): In_channel.t =
  In_channel.create filename

let parse_test (contents: In_channel.t): Ast.ast option =
  Lexing.from_channel contents
  |> parse_with_error

let compile filename =
  let open Shell.Process in
  try
    let command = cmd "./elc" ["-no-color"; filename] in
    run command content_and_stderr
  with
  | Failed r -> (r.stdout, r.stderr)

let execute input source =
  let open Shell.Process in
  Shell.run "./elc" [source];
  let (base, _) = Filename.split_extension source in
  let basename = Filename.basename base in
  print_string basename;
  let command = cmd ~input:input ("./" ^ basename) [] in
  let output = Shell.run_full ~input:input ("./" ^ basename) [] in
  Shell.rm ("./" ^ basename); output

let perform_action filename action =
  match action with
  | Ast.Compile ->
    printf "compiling %s" filename;
    compile filename
  | Ast.RunWithInput s ->
    printf "running %s with input %S" filename s;
    execute s filename

let verify_expectation results (subject, assertion) =
  match subject with
  | Ast.Stderr -> ()
  | Ast.Stdout -> ()

let execute_test filename (action, expectations) =
    let results = perform_action filename action in
    List.iter expectations ~f:(verify_expectation results)

let execute_tests filename (test: Ast.ast) =
  List.iter test ~f:(execute_test filename)

let process_example_file dir filename =
  let filename = Filename.concat dir filename in
  let contents = load_file filename in
  let Some test_case = parse_test contents in
  execute_tests filename test_case


let all_files dir =
  Sys.readdir dir
  |> Array.to_list


let () =
  all_files "example"
  |> List.iter ~f:(process_example_file "example")
