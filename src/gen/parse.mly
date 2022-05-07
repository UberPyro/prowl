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
  | list(ty_term) EFFECT list(ty_term) {$1, $3}
  | nonempty_list(ty_term) {[], $1}

ty_term: 
  | ID {TId $1}
  | CAP {TGen $1}
  | ty_term DOT ID {TAccess ($1, $3)}
  | LBRACE RBRACE {assert $2 == Rel; TCapture []}
  | LBRACE ty_eff RBRACE {assert $3 == Rel; TCapture $2}
  | LBRACK separated_list(COMMA, ty_eff) RBRACK {TList $2}
  | LBRACK separated_list(
    COMMA, 
    separated_pair(ty_term, ASSIGN, ty_eff)
  ) RBRACK {TMap (fst $2, snd $2)}

