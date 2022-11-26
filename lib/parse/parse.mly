%{
  open Batteries
  open Ast
  open Meta
  open Type
%}

// %token
//   SIG MOD END OPEN MIX FN TY DATA
//   DOT COLON

// %token<string> STACK_ID CAP_ID ID

%token
  LPAREN RPAREN
  LBRACK RBRACK

%token<int> INT
%token<float> FLOAT
%token<char> CHAR
%token<string> STRING


%%

expr: 
  | INT {exnode (EInt $1) $loc (mono tyint)}
  | FLOAT {exnode (EFloat $1) $loc (mono tyfloat)}
  | CHAR {exnode (EChar $1) $loc (mono tychar)}
  | STRING {exnode (EString $1) $loc (mono tystring)}
  | LPAREN expr RPAREN {}
