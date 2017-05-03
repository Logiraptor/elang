%token <int> INT
%token <string> ID
%token LET
%token EQUAL
%token COMMA
%token LPAREN
%token RPAREN

%token EOF

%start <Ast.ast> prog
%%

prog:
  | l = nonempty_list(func) EOF { l };

func:
  | LET id=ID LPAREN a=args RPAREN EQUAL v=value { (id, a, v) }

args:
  | l = separated_list(COMMA, ID) { l };

value:
  | i = INT { Ast.INT i };
