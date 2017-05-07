# ELANG

The general structure of this codebase is as follows:

1. Parser (Menhir) (Source text -> AST | ParseError)
1. Loader -> Module | TypeError
1. Compile (Module -> LLVM IR)
1. Interp (LLVM IR -> Machine Code)

## TODO:

Small goal
make strings a thing

Medium goal
make `printf("Hello, %d!", 42)` valid elang

Big goal:
general `extern` functionality for integration with libc

linking c functions (on hold until more builtins exist for `extern` definitions)
    - print_int (builtin)
    - printf (externed)

sequencing (let bindings most likely)

additional built-ins:
    - strings
    - arrays
    - structures
    - lambdas