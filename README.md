# ELANG

[![Build Status](https://travis-ci.org/Logiraptor/elang.svg?branch=master)](https://travis-ci.org/Logiraptor/elang)
[![Coverage Status](https://coveralls.io/repos/github/Logiraptor/elang/badge.svg?branch=master)](https://coveralls.io/github/Logiraptor/elang?branch=master)

Functional language optimized for speed of execution

The general structure of this codebase is as follows:

1. Parser (Menhir) (Source text -> AST | ParseError)
1. Loader -> Module | TypeError
1. Compile (Module -> LLVM IR)
1. Interp (LLVM IR -> Machine Code)
