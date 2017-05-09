%token <int> INT
%token <Ast.symbol> ID
%token LET
%token IN
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
%token MOD
%token <string> STRING
%token EXTERN
%token TYPE
%token AND

%right ELSE
%right IN
%left AND
%left EQUAL
%left PLUS MINUS
%left MOD
%left TIMES
%right LPAREN

%token EOF

%start <Ast.ast> prog
%%

prog:
  | l = nonempty_list(decl) EOF { l };

decl:
  | f=func { Ast.FuncDecl f }
  | e=extern { Ast.ExternDecl e }
  | t=typ_decl { Ast.TypeDecl t }

extern:
  | EXTERN t=typ id=ID LPAREN a=args RPAREN { (id, a, t) }

func:
  | LET t=typ id=ID LPAREN a=args RPAREN EQUAL v=value { (id, a, v, t) };

typ_decl:
  | TYPE id=ID EQUAL t=typ { (id, t) }

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
  | a=value o=op b=value {Ast.BinOp (o, a, b)}  
  | f=value LPAREN p=params RPAREN {Ast.Apply (f, p) }
  | IF cond=value THEN conseq=value ELSE alt=value {Ast.If (cond, conseq, alt)}
  | LET name=ID EQUAL v=value IN body=value {Ast.Let (name, v, body)}

%inline op:
  | PLUS {Ast.Add}
  | TIMES {Ast.Mul}
  | MINUS {Ast.Sub}
  | MOD {Ast.Mod}
  | EQUAL {Ast.Equal}
  | AND {Ast.And}