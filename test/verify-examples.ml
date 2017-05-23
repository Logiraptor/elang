open Core.Std
open Core_extended

(*$(locker) $(oce) bisect-ppx-report -I _build/ -html coverage/ -text coverage.txt bisect*.out*)


let all_files () =
  "examples"
  |> Sys.readdir
  |> Array.to_list


let is_example_file name =
  let (base, ext) = Filename.split_extension name in
  match ext with
  | Some "el" -> true
  | _ -> false

let example_files () =
  List.filter ~f:is_example_file (all_files ())

type expectation =
  | Output of (string * string)
  | CompileError of string
  | Unknown of string

type test_case = {
  example : string;
  result : expectation
}

let load_expectation source =
  let output_file_name = Filename.concat "examples" (source ^ "." ^ "output") in
  let input_file_name = Filename.concat "examples" (source ^ "." ^ "input") in
  let error_file_name = Filename.concat "examples" (source ^ "." ^ "error") in    
  let input_contents = if Sys.file_exists_exn input_file_name then In_channel.read_all input_file_name else "" in
  let has_output = Sys.file_exists_exn output_file_name in
  let has_error = Sys.file_exists_exn error_file_name in
  match (has_output, has_error) with
  | (true, false) -> Output (output_file_name, input_contents)
  | (false, true) -> CompileError error_file_name
  | _ -> Unknown (sprintf "%s should have either an error file or output file\n" source)

let load_test_case source =
  (source, load_expectation source)

let test_cases () =
  List.map ~f:load_test_case (example_files ())

let compile source =
  let open Shell.Process in
  try
    Shell.run_full "./elc" ["-no-color"; Filename.concat "examples" source]
  with
  | Failed r -> r.stderr

let execute input source =
  Shell.run "./elc" [Filename.concat "examples" source];
  let (base, _) = Filename.split_extension source in
  let output = Shell.run_full ~input:input ("./" ^ base) [] in
  Shell.rm ("./" ^ base); output

type test_result =
  | Error of string
  | Diff of string
  | Pass

let diff file contents =
  let diff_result = Shell.run_full ~expect:[0;1] ~input:contents "diff" ["-u"; file; "-"] in
  match diff_result with
  | "" -> Pass
  | s -> Diff s

let validate_test_case (source, expectation) =
  match expectation with
  | Output (output_file_name, input) ->
    let output = execute input source in
    diff output_file_name output
  | CompileError s ->
    let output = compile source in
    diff s output
  | Unknown s -> Error s

let print_result r =
  match r with
  | Error e -> print_string e; exit 1
  | Diff s -> printf "Diff:\n%s" s; exit 1
  | Pass -> print_string "."

let results () = List.map ~f:validate_test_case (test_cases ())

let _ = 
  try
    List.iter ~f:print_result (results ())
  with Shell.Process.Failed r ->
    let open Shell.Process in
    print_string r.stderr


(*#!/bin/bash

  set -e

  function runExample {
    set +e
    ./output << DOC > $1
  input
  DOC
    exit_status=$?
    set -e
    if [ 0 != $exit_status ]; then
        echo "example returned non-zero exit code: $exit_status"
        exit $exit_status
    fi
  }

  for f in ./examples/*.el; do
    expectedOutput="$f.output"
    actualOutput=$(mktemp)
    if [ ! -f $expectedOutput ]; then
        echo "Missing output file $expectedOutput"
        exit 1
    fi
    echo "Compiling $f"
    ./elc $f
    echo "Running $f"
    runExample $actualOutput
    echo "Comparing output of $f"
    diff -u $expectedOutput $actualOutput
  done

  echo "Success"*)