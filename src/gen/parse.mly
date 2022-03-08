%{
  open Batteries

  module M = struct
    'a t = 'a * Parsing.position
  end
  
  module T = Ast(M)
%}

%token
  FN VAL LET IN OPEN MIX USE INST
  PUB OPAQ TYPE NEW DATA
  MOD SIG BEGIN END WITH
  TO AS

  PLUS MINUS TIMES DIV
  EXPR CONS SNOC APPEND
  RANGE CMP EQ NEQ

  LANGLE   RANGLE
  LPAREN   RPAREN
  LBRACKET RBRACKET
  LBRACE   RBRACE

  PBRACE ABRACE PABRACE
  USCORE BLANK EOF

%token<string>
  STR SUM ATOM
  LABEL METATYPE ID
  ACCESS PACCESS MACRO

%token<int> INT
%token<float> FLOAT
%token<char> CHAR

%left CMP
%left EQ NEQ
%left PLUS MINUS
%left TIMES DIV

%start<T.t> program

%nonassoc ACCESS PACCESS MACRO

%%

program: 
  | stmt EOF {SProg $1}
  | expr EOF {EProg $1}


