# ELANG

The general structure of this codebase is as follows:

1. Parser (Menhir) (Source text -> AST | ParseError)
1. Loader -> Module | TypeError
1. Compile (Module -> LLVM IR)
1. Interp (LLVM IR -> Machine Code)

## TODO:

linking c functions
    - print_int (builtin)
    - printf (externed)

branches
sequencing

additional built-ins:
    - bools
    - strings
    - arrays
    - lambdas
    - structures