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

%token EOF

%start <VerifyAst.ast> prog
%%

prog:
  | l = nonempty_list(testcase) EOF { l };

testcase:
  | a=action l=list(expectation) { (a, l) }

action:
  | COMPILE file=STRING { VerifyAst.Compile file }
  | EXECUTE file=STRING i=option(input) { VerifyAst.run_with_input file i }

input:
  | WITH INPUT s=STRING { s }

expectation:
  | EXPECT s=subject TO a=assertion { (s, a) }

subject:
  | STDOUT { VerifyAst.Stdout }
  | STDERR { VerifyAst.Stderr }

assertion:
  | CONTAIN s=STRING { VerifyAst.Contains s }
  | EQUAL s=STRING { VerifyAst.Equals s }
