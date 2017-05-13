# ELANG

Functional language optimized for speed of execution

The general structure of this codebase is as follows:

1. Parser (Menhir) (Source text -> AST | ParseError)
1. Loader -> Module | TypeError
1. Compile (Module -> LLVM IR)
1. Interp (LLVM IR -> Machine Code)

## TODO:

Small goal
structure definition order should not matter

Medium goal
functional structure update (copy on write)

Big goal:
compound structure support (arrays / structs)

establish end-to-end tests w/ coverage

automatic reference counting gc

High quality error messages outside of parsing. (General error marshalling module)

linking c functions (on hold until more builtins exist for `extern` definitions)
    - print_int (builtin)
    - printf (externed)

additional built-ins:
    - arrays
    - lambdas