%token EXPECT
%token COMPILATION
%token TO
%token PRINT
%token ERROR
%token <string> STRING
%token EXECUTION
%token WITH
%token INPUT

%token EOF

%start <Ast.ast> prog
%%

prog:
  | l = nonempty_list(testcase) EOF { l };

testcase:
  | EXPECT a=action TO PRINT o=output { (a, o) }

action:
  | COMPILATION { Ast.Compile }
  | EXECUTION i=input { Ast.RunWithInput i }

input:
  | WITH INPUT s=STRING { s }
  | empty { "" }

output:
  | ERROR s=STRING { Ast.StdErr s }
  | s=STRING { Ast.StdOut s }
