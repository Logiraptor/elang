# ELANG

The general structure of this codebase is as follows:

1. Parser (Menhir) (Source text -> AST | ParseError)
1. Types Ast -> TypedAst | TypeError
1. CodeGen (TypedAst -> TypedIR)
1. Interp (TypedIR -> Result)
1. Compile (TypedIR -> LLVM IR)
1. LLVM (LLVM IR -> Machine Code)

