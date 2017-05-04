%token <int> INT
%token <string> ID
%token LET
%token EQUAL
%token COMMA
%token LPAREN
%token RPAREN
%token PLUS

%token EOF

%start <Ast.ast> prog
%%

prog:
  | l = nonempty_list(func) EOF { l };

func:
  | LET id=ID LPAREN a=args RPAREN EQUAL v=value { (id, a, v) };

args:
  | l = separated_list(COMMA, arg) { l };

arg:
  | id=ID t=typ { (t,id) };

typ:
  | id=ID { Ast.NamedType id };

params:
  | l=separated_list(COMMA, value) { l };

value:
  | i = INT { Ast.Int i }
  | id=ID {Ast.ID id}
  | a=value PLUS b=value {Ast.BinOp (Ast.Add, a, b)}
  | f=value LPAREN p=params RPAREN {Ast.Apply (f, p) }
