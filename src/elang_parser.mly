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
%token LCURLY
%token RCURLY
%token PLUS
%token TIMES
%token MINUS
%token MOD
%token <string> STRING
%token EXTERN
%token TYPE
%token AND
%token STRUCT
%token DOT


%right ELSE
%right IN
%left AND
%left EQUAL
%left PLUS MINUS
%left MOD
%left TIMES
%right LPAREN
%left DOT

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
  | LET t=typ id=ID LPAREN a=args RPAREN EQUAL v=value_with_pos { (id, a, v, t) };

typ_decl:
  | TYPE id=ID EQUAL t=typ { (id, t) }

args:
  | l = separated_list(COMMA, arg) { l };

arg:
  | id=ID t=typ { (t,id) };

typ:
  | id=ID { Ast.NamedType id }
  | STRUCT LPAREN a=args RPAREN { Ast.StructType a }

params:
  | l=separated_list(COMMA, value_with_pos) { l };

%inline value_with_pos:
  | v=value { Ast.capture_pos (Ast.make_region $startpos $endpos) v }

value:
  | v=value_with_pos DOT id=ID {Ast.FieldAccess (v, id) }
  | i = INT { Ast.Int i }
  | id=ID {Ast.ID id}
  | str=STRING {Ast.String str}
  | a=value_with_pos o=op b=value_with_pos {Ast.BinOp (o, a, b)}  
  | f=value_with_pos LPAREN p=params RPAREN {Ast.Apply (f, p) }
  | IF cond=value_with_pos THEN conseq=value_with_pos ELSE alt=value_with_pos {Ast.If (cond, conseq, alt)}
  | LET name=ID EQUAL v=value_with_pos IN body=value_with_pos {Ast.Let (name, v, body)}
  | LCURLY fields=separated_list(COMMA, field) RCURLY { Ast.StructLit fields }

field:
  | id=ID EQUAL v=value_with_pos { (id, v) }

%inline op:
  | PLUS {Ast.Add}
  | TIMES {Ast.Mul}
  | MINUS {Ast.Sub}
  | MOD {Ast.Mod}
  | EQUAL {Ast.Equal}
  | AND {Ast.And}