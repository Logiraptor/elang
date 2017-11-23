open Core.Std
open Lexing
open Core_extended

module Messages = struct 
  let message n = "SYNTAX ERROR"
end

module P = struct
  include Verify_parser.Incremental
  type ast = VerifyAst.ast
  type 'a checkpoint = 'a Verify_parser.MenhirInterpreter.checkpoint
end

module L = struct
  include Verify_lexer
  type token = Verify_parser.token
end

module Parser = Syntax_analyzer.Make(Messages)(Verify_parser.MenhirInterpreter)(P)(L)

module Dump = struct
  let dump_string s =
    if (String.contains s '\n') || (String.length s > 10) then
      sprintf "\n<<<\n%s\n>>>" s
    else
      sprintf "'%s'" s

  let dump_action (action: VerifyAst.action) =
    match action with
    | VerifyAst.Compile file -> sprintf !"COMPILE '%s'" file
    | VerifyAst.RunWithInput (file, input) -> sprintf !"RUN '%s' WITH INPUT %{dump_string}" file input 

  let dump_assertion (assertion: VerifyAst.assertion) =
    match assertion with
    | VerifyAst.Equals value -> sprintf !"EQUAL %{dump_string}" value
    | VerifyAst.Contains value -> sprintf !"CONTAIN %{dump_string}" value

  let dump_subject (subject: VerifyAst.subject) =
    match subject with
    | VerifyAst.Stderr -> "STDERR"
    | VerifyAst.Stdout -> "STDOUT"

  let dump_expectation ((subject, assertion): VerifyAst.expectation) =
    sprintf "EXPECT %s TO %s" (dump_subject subject) (dump_assertion assertion)

  let dump_expectations expectations =
    List.map ~f:dump_expectation expectations
    |> String.concat ~sep:"\n"

  let dump_test ((action, expectations): VerifyAst.testcase) =
    (dump_action action) ^ "\n" ^ (dump_expectations expectations)

  let dump_tests tests =
    List.map ~f:dump_test tests
    |> String.concat ~sep:"\n\n"
end

let should_trust_results = ref false

type test_result = 
  | Success 
  | Error of (string * string)

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
  | VerifyAst.Compile source ->
    let realpath = rel_path filename source in
    (sprintf "-- compiling %s\n" realpath, compile realpath)
  | VerifyAst.RunWithInput (source, s) ->
    let realpath = rel_path filename source in  
    (sprintf "-- running %s with input %S\n" realpath s, execute s realpath)

let write_temp contents = 
  let filename = Filename.temp_file "" "" in
  Out_channel.write_all filename contents; filename

let diff expected actual =
  let file = write_temp expected in
  let diff = Shell.run_full ~expect:[0;1] ~input:actual "diff" ["-u"; file; "-"] in
  Shell.rm file; diff

let verify_assertion title subject (assertion: VerifyAst.assertion): test_result =
  match assertion with
  | VerifyAst.Contains s -> 
      if String.is_substring ~substring:s subject then Success
      else Error (title, sprintf "Expected %S to contain %S" subject s)
  | VerifyAst.Equals s ->
      if String.equal subject s then Success
      else Error (title, sprintf "Expected no diff, got:\n\n%s\n\nwhole actual:\n%s\n" (diff subject s) subject)

let verify_expectation title (stdout, stderr) (subject, assertion): test_result =
  match subject with
  | VerifyAst.Stderr -> verify_assertion title stderr assertion
  | VerifyAst.Stdout -> verify_assertion title stdout assertion

let gather_result filename (action, _) : VerifyAst.testcase =
  let (title, results) = perform_action filename action in
  let (stdout, stderr) = results in
    (action, [
      (VerifyAst.Stdout, VerifyAst.Equals stdout);
      (VerifyAst.Stderr, VerifyAst.Equals stderr)
    ])

let gather_results filename (test: VerifyAst.ast) =
  List.map test ~f:(gather_result filename)

let execute_test filename (action, expectations) =
  let (title, results) = perform_action filename action in
  List.map expectations ~f:(verify_expectation title results)

let execute_tests filename (test: VerifyAst.ast) =
  List.map test ~f:(execute_test filename)

let print_result (result: test_result) =
  let open ANSITerminal in
  match result with
  | Success -> print_string [ Foreground Green ] "."
  | Error (title, msg) ->
    print_string [Bold] title;
    print_string [  ] msg

let print_results (results: test_result list) =
  List.iter ~f:print_result results

let process_example_file dir filename =
  let filename = Filename.concat dir filename in
  let parse_result = load_file filename in
  match parse_result with
  | Ok test_case ->
    (if !should_trust_results then
      let results = gather_results filename test_case in    
      Out_channel.write_all filename (Dump.dump_tests results)
    else
      let results = execute_tests filename test_case in
      print_results (List.concat results))
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

let run trust_results () =
  should_trust_results := trust_results;
  all_example_files "example"
  |> List.iter ~f:(process_example_file "example")

let () =
  Command.basic ~summary:"verify elang compiler examples"
    Command.Spec.(
      empty
      +> flag "-trust-results" no_arg ~doc:" overwrite expectations with actual results"
    )
    run
  |> Command.run
