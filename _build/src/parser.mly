%token <int> INT
%token EOF

%start <int option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }
  ;

value:
  | i = INT { i };