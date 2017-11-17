open Core.Std
open Lexing
open Verification
open Core_extended

module Messages = struct 
  let message n = "SYNTAX ERROR"
end

module P = struct
  include Verify_parser.Incremental
  type ast = Verification.Ast.ast
  type 'a checkpoint = 'a Verify_parser.MenhirInterpreter.checkpoint
end

module L = struct
  include Verify_lexer
  type token = Verify_parser.token
end

module Parser = Syntax_analyzer.Make(Messages)(Verify_parser.MenhirInterpreter)(P)(L)


type test_result = Success | Error of (string * string)

let load_file (filename: string) =
  let open Result in
  FileResolver.resolve filename
  >>= Parser.parse_file

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
  let command = cmd ("./" ^ basename) [] in
  let output = run ~input:input command content_and_stderr in
  Shell.rm ("./" ^ basename); output

let rel_path from path =
  Filename.concat (Filename.dirname from) path

let perform_action filename action =
  match action with
  | Ast.Compile source ->
    let realpath = rel_path filename source in
    (sprintf "-- compiling %s\n" realpath, compile realpath)
  | Ast.RunWithInput (source, s) ->
    let realpath = rel_path filename source in  
    (sprintf "-- running %s with input %S\n" realpath s, execute s realpath)

let write_temp contents = 
  let filename = Filename.temp_file "" "" in
  Out_channel.write_all filename contents; filename

let diff expected actual =
  let file = write_temp expected in
  let diff = Shell.run_full ~expect:[0;1] ~input:actual "diff" ["-u"; file; "-"] in
  Shell.rm file; diff

let verify_assertion title subject (assertion: Ast.assertion): test_result =
  match assertion with
  | Ast.Contains s -> 
      if String.is_substring ~substring:s subject then Success
      else Error (title, sprintf "Expected %S to contain %S" subject s)
  | Ast.Equals s ->
      if String.equal subject s then Success
      else Error (title, sprintf "Expected no diff, got:\n\n%s\n" (diff subject s))

let verify_expectation title (stdout, stderr) (subject, assertion): test_result =
  match subject with
  | Ast.Stderr -> verify_assertion title stderr assertion
  | Ast.Stdout -> verify_assertion title stdout assertion

let execute_test filename (action, expectations) =
    let (title, results) = perform_action filename action in
    List.map expectations ~f:(verify_expectation title results)

let execute_tests filename (test: Ast.ast) =
  List.map test ~f:(execute_test filename)

let print_result (result: test_result) =
  let open ANSITerminal in
  match result with
| Success -> print_string [ Foreground Green ] "."
| Error (title, msg) ->
  print_string [Bold] title;
  print_string [  ] msg

let process_example_file dir filename =
  let () = printf "Running test %s\n" filename in
  let filename = Filename.concat dir filename in
  let parse_result = load_file filename in
  match parse_result with
  | Ok test_case ->
    let results = execute_tests filename test_case in
    List.iter (List.concat results) ~f:print_result
  | Error error ->
    print_string error

let is_example_file name =
  let (base, ext) = Filename.split_extension name in
  match ext with
  | Some "eltest" -> true
  | _ -> false

let all_example_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter ~f:is_example_file

let () =
  all_example_files "example"
  |> List.iter ~f:(process_example_file "example")
