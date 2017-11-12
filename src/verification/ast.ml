open Core.Std

type action = Compile | RunWithInput of string
[@@deriving sexp]

type output = Stderr of string | Stdout of string
[@@deriving sexp]

type testcase = (action * output)
[@@deriving sexp]

type ast = testcase list
[@@deriving sexp]

