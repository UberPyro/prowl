%{
  open Batteries
  open AST

  open Lex_proc
  open Util

  let ty_conv = function
    | Some st -> st
    | None -> TCat []
%}

%token
  DEF OPEN MIX USE IMPL SIG
  PUB OPAQ TYPE ALIAS CLASS

  PLUS MINUS TIMES DIVIDE
  EXP RANGE SNOC CONS
  APPEND BIND ALT CAT
  INTERSECT

  CMP EQ NEQ LT LE GT GE

  COLON TILDE ASSIGN ARROW
  COMMA SEMICOLON DOT

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
%left DOT

%start<program> program

%%

%inline access: 
  | PUB {Pub}
  | OPAQ {Opaq}

sp: 
  | DEF ID ASSIGN ty {SDef ($2, $4)}
  | DEF ID ioption(preceded(ASSIGN, ty)) {STy ($2, $3)}

%inline ty: constr_list ty_eff {$1, $2}
%inline constr_list: ID list(CAP) CONSTRAINT {$1, $2}

ty_eff: 
  | ioption(ty_stack) EFFECT ioption(ty_stack)
    {ty_conv $1, ty_conv $3}
  | ty_stack {TCat [], $1}

%inline ty_stack: 
  | ty_term {$1}
  | pop_list_ge_2(ty_term) {TCat $1}
  | separated_nonempty_list(
    SEMICOLON, 
    pair(nonempty_list(ty_term), CAP)
  ) {TData (let+ x = $1 in [x])}
  | LBRACE separated_nonempty_list(
    SEMICOLON, 
    separated_nonempty_list(
      COMMA, 
      pair(nonempty_list(ty_term), CAP)
    )
  ) RBRACE {assert $3 == Rel; TData $2}

ty_term: 
  | ID {TId $1}
  | CAP {TGen $1}
  | ty_term DOT ID {TAccess ($1, $3)}
  | LBRACE RBRACE {assert $2 == Rel; TCapture []}
  | LBRACE ty_eff RBRACE {assert $3 == Rel; TCapture $2}
  | LBRACK separated_list(COMMA, ty_eff) RBRACK {TList $2}
  | LBRACK separated_list(
    COMMA, 
    separated_pair(ty_stack, ASSIGN, ty_eff)
  ) RBRACK {TMap (fst $2, snd $2)}
  | LT GT {TUnit}
  | LT SEMICOLON GT {TVoid}
  | separated_nonempty_list(
    SEMICOLON, 
    separated_nonempty_list(COMMA, ty_eff)
  ) {TBin $1}
  | MOD list(sp) END {TMod $2}
  | SIG list(sp) END {TSig $2}

%inline pop_list_ge_2(entry): 
  | entry nonempty_list(entry) {$1 :: $2}

%inline sep_pop_list_ge_2(sep, entry): 
  | entry sep separated_nonempty_list(sep, entry) {$1 :: $3}

s: 
  | access DEF p ioption(preceded(COLON, ty)) ASSIGN e
    {Def ($1, $3, $6, $4)}
  | OPEN e {Open $2}
  | USE e {Use $2}
  | MIX e {Mix $2}
  | IMPL MIX e {MixImpl $3}
  | access TYPE ID separated_pair(list(GENERIC), ASSIGN, ty_eff)
    {Ty ($1, $3, $4)}
  