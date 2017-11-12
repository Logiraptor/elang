%token COMPILE
%token EXECUTE
%token EXPECT
%token STDOUT
%token STDERR
%token TO
%token EQUAL
%token CONTAIN
%token <string> STRING
%token WITH
%token INPUT
%token DOT
%token START_EXAMPLE
%token END_EXAMPLE

%token EOF

%start <Ast.ast> prog
%%

prog:
  | START_EXAMPLE l = nonempty_list(testcase) END_EXAMPLE EOF { l };

testcase:
  | a=action l=list(expectation) { (a, l) }

action:
  | COMPILE { Ast.Compile }
  | EXECUTE i=option(input) { Ast.run_with_input i }

input:
  | WITH INPUT s=STRING { s }

expectation:
  | EXPECT s=subject TO a=assertion { (s, a) }

subject:
  | STDOUT { Ast.Stdout }
  | STDERR { Ast.Stderr }

assertion:
  | CONTAIN s=STRING { Ast.Contains s }
  | EQUAL s=STRING { Ast.Equals s }
