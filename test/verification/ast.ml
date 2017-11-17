open Core.Std

type subject = Stderr | Stdout
[@@deriving sexp]

type assertion = Contains of string | Equals of string
[@@deriving sexp]

type expectation = (subject * assertion)
[@@deriving sexp]

type action = Compile of string | RunWithInput of (string * string)
[@@deriving sexp]

type testcase = (action * expectation list)
[@@deriving sexp]

type ast = testcase list
[@@deriving sexp]

let run_with_input (filename: string) (s: string option) : action =
    match s with
    | None -> RunWithInput (filename, "")
    | Some text -> RunWithInput (filename, text)
