%token <int> INT
%token <Ast.symbol> ID
%token LET
%token IF
%token THEN
%token ELSE
%token EQUAL
%token COMMA
%token LPAREN
%token RPAREN
%token PLUS
%token TIMES
%token MINUS
%token <string> STRING
%token EXTERN

%token EOF

%start <Ast.ast> prog
%%

prog:
  | l = nonempty_list(decl) EOF { l };

decl:
  | f=func { Ast.FuncDecl f }
  | e=extern { Ast.ExternDecl e }

extern:
  | EXTERN t=typ id=ID LPAREN a=args RPAREN { (id, a, t) }

func:
  | LET t=typ id=ID LPAREN a=args RPAREN EQUAL v=value { (id, a, v, t) };

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
  | str=STRING {Ast.String str}
  | a=value PLUS b=value {Ast.BinOp (Ast.Add, a, b)}
  | a=value TIMES b=value {Ast.BinOp (Ast.Mul, a, b)}
  | a=value MINUS b=value {Ast.BinOp (Ast.Sub, a, b)}
  | a=value EQUAL b=value {Ast.BinOp (Ast.Equal, a, b)}  
  | f=value LPAREN p=params RPAREN {Ast.Apply (f, p) }
  | IF cond=value THEN conseq=value ELSE alt=value {Ast.If (cond, conseq, alt)}
