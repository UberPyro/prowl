%{
  open Batteries
  open AST

  open Lex_proc
  open Util
%}

%token
  DEF OPEN MIX USE IMPL SIG
  PUB OPAQ TYPE ALIAS CLASS

  PLUS MINUS TIMES DIVIDE
  EXP RANGE SNOC CONS
  APPEND BIND ALT CAT

  CMP EQ NEQ LT LE GT GE

  COLON TILDE ASSIGN ARROW
  COMMA SEMICOLON

  USCORE BLANK EOF

  LPAREN RPAREN
  LBRACK RBRACK
  LBRACE

  NONCAP_BRACK ATOM_BRACK TIMES_BRACK
  DO MOD END UNIT

%token<string>
  LET AND AS STR
  ID CAP SYMBOL INFIX

%token<int> INT
%token<float> FLOAT
%token<char> CHAR

%token<e> COMB QUANT
%token<greed> RBRACE

%left CAT
%left ALT
%left BIND
%right RANGE
%left SNOC
%left APPEND
%right CONS
%left INFIX
%left PLUS MINUS
%left TIMES DIVIDE
%right EXP

%start <program> program

%%
