%token <int> INT
%token <string> ID
%token LET
%token EQUAL

%token EOF

%start <Ast.ast> prog
%%

prog:
  | l = nonempty_list(func) EOF { l };

func:
  | LET id=ID EQUAL v=value { (id, [], v) }

value:
  | i = INT { Ast.INT i };
