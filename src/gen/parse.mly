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

  CMP EQ NEQ LT LE GT GE

  COLON TILDE ASSIGN ARROW
  COMMA DOT INTERSECT

  USCORE BLANK EOF
  CONSTRAINT EFFECT

  LPAREN RPAREN
  LBRACK RBRACK
  LBRACE

  NONCAP_BRACK ATOM_BRACK
  INV_BRACK TIMES_BRACK
  DO MOD END UNIT

%token<string>
  LET AND AS STR
  ID CAP SYMBOL INFIX

%token<int> INT
%token<float> FLOAT
%token<char> CHAR

%token<e> COMB
%token<quant * greed> QUANT
%token<greed> SEMICOLON RBRACE

%left CAT
%left COLON
%left ALT
%left BIND
%left RANGE
%left SNOC
%left APPEND
%right CONS
%left INFIX
%left PLUS MINUS
%left TIMES DIVIDE
%right EXP
%left TILDE
%left DOT

%start<program> program

%%

program: access e EOF {$1, $2}

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
    semi, 
    pair(nonempty_list(ty_term), CAP)
  ) {TData (let+ x = $1 in [x])}
  | LBRACE sep_pop_list_ge_2(  // consider removing this one
    semi,                      // so it lines up with exprs
    separated_nonempty_list(
      COMMA, 
      pair(nonempty_list(ty_term), CAP)
    )
  ) rbrace {TData $2}
  | LBRACE separated_nonempty_list(
    semi, 
    sep_pop_list_ge_2(
      COMMA, 
      pair(nonempty_list(ty_term), CAP)
    )
  ) rbrace {TData $2}

ty_term: 
  | ID {TId $1}
  | CAP {TGen $1}
  | ty_term DOT ID {TAccess ($1, $3)}
  | LBRACE rbrace {TCapture []}
  | LBRACE ty_eff rbrace {TCapture $2}
  | LBRACK separated_list(COMMA, ty_eff) RBRACK {TList $2}
  | LBRACK separated_list(
    COMMA, 
    separated_pair(ty_stack, ASSIGN, ty_eff)
  ) RBRACK {TMap (fst $2, snd $2)}
  | LT GT {TUnit}
  | LT semi GT {TVoid}
  | separated_nonempty_list(
    semi, 
    separated_nonempty_list(COMMA, ty_eff)
  ) {TBin $1}
  | MOD list(sp) END {TMod $2}
  | SIG list(sp) END {TSig $2}

%inline pop_list_ge_2(entry): 
  | entry nonempty_list(entry) {$1 :: $2}

%inline sep_pop_list_ge_2(sep, entry): 
  | entry sep separated_nonempty_list(entry) {$1 :: $3}

s: 
  | access DEF p ioption(preceded(COLON, ty)) ASSIGN e
    {Def ($1, $3, $6, $4)}
  | OPEN e {Open $2}
  | USE e {Use $2}
  | MIX e {Mix $2}
  | IMPL MIX e {MixImpl $3}
  | access TYPE ID separated_pair(list(CAP), ASSIGN, ty_eff)
    {Ty ($1, $3, $4)}

e: 
  | bop {Sect $1}
  | bop bexp {SectLeft ($1, $2)}
  | bexp bop {SectRight ($1, $2)}
  | bexp {$1}
  | let_body(LET) list(let_body(AND)) ARROW e {Let ($1 :: $2, $4)}
  | AS p ARROW e {As ($1, $2, $4)}

%inline let_body(kw): 
  | kw p ASSIGN e {$1, $2, $4}

bexp: 
  | e bop e {Bop ($1, $2, $3)}
  | nonempty_list(term) {
    match $1 with
    | [h] -> h
    | lst -> Cat lst
  }

term: 
  | ID {Id $1}

  | term DOT ID {Access ($1, $3)}
  | term DOT LBRACK e RBRACK {Get ($1, $4)}
  | term TILDE term {Span ($1, $3)}

  | INT {Int $1}
  | FLOAT {Flo $1}
  | CHAR {Char $1}
  | STR {Str $1}
  | UNIT {Unit}

  | LPAREN RPAREN {Cat []}
  | LPAREN e RPAREN {Cap e}
  | DO e END {e}
  | LPAREN semi RPAREN {Inv []}
  | LBRACK e nonempty_list(pair(SEMICOLON, e)) RBRACK {Case ($1, $2)}
  | INV_BRACK separated_list(semi, e) RBRACK {Inv $2}

  | LBRACK separated_list(COMMA, e) RBRACK {List $2}
  | LBRACK separated_list(
    COMMA, 
    separated_pair(e, ASSIGN, e)
  ) RBRACK {Map $2}
  | LPAREN list(semi) sep_pop_list_ge_2(COMMA, e)
    list(semi) RPAREN {Bin (List.length $2, $3, List.length $4)}
  | LPAREN nonempty_list(semi) separated_nonempty_list(COMMA, e)
    list(semi) RPAREN {Bin (List.length $2, $3, List.length $4)}
  | LPAREN list(semi) separated_nonempty_list(COMMA, e)
    nonempty_list(semi) RPAREN {Bin (List.length $2, $3, List.length $4)}
  
  | CAP {Data $1}
  | LBRACE sep_pop_list_ge_2(COMMA, e) rbrace {Prod $2}
  | MOD list(s) END {Mod $2}
  | LBRACE e rbrace {Capture $2}

  | SYMBOL {Sym $1}
  | COMB {$1}
  | e QUANT {Quant ($1, fst $2, snd $2)}
  | e TIMES_BRACK e RBRACE {$1, Num $3, $4}
  | e TIMES_BRACK e COMMA RBRACE {$1, Min $3, $5}
  | e TIMES_BRACK COMMA e RBRACE {$1, Max $4, $5}
  | e TIMES_BRACK e COMMA e RBRACE {$1, Range ($3, $5), $6}

  | NONCAP_BRACK e RPAREN {Noncap $2}
  | ATOM_BRACK e RPAREN {Atomic $2}

%inline bop: 
  | PLUS {"+"} | MINUS {"-"} | TIMES {"*"} | DIVIDE {"/"} 
  | EXP {"**"} | RANGE {".."} | SNOC {">-"} | CONS {"-<"}
  | APPEND {"++"} | BIND {">>="} | ALT {"|"} | CAT {"&"}
  | INTERSECT {"&&"} | INFIX {$1}

%inline semi: SEMICOLON {assert $1 == Gre; ";"}
%inline rbrace: RBRACE {assert $1 == Gre}

p: 
  | p p_bop p {PBop ($1, $2, $3)}
  | p COLON ty {PAsc ($1, $3)}
  | nonempty_list(p_term) {
    match $1 with
    | [h] -> h
    | lst -> PCat lst
  }

p_term: 
  | ID {PId $1}
  | p_term DOT ID {PAccess ($1, $3)}
  | USCORE {PBlank}
  | BLANK {PBlank}
  | OPEN {POpen}
  | USE {PUse}

  | INT {PInt $1}
  | FLOAT {PFlo $1}
  | STR {PStr $1}
  | CHAR {PChar $1}
  | LT GT {PUnit $1}
  
  | LBRACK separated_list(COMMA, p) RBRACK {PList $2}
  | LBRACK separated_list(
    COMMA, 
    separated_pair(e, ASSIGN, p)
  ) RBRACK {PMap $2}
  | LPAREN list(semi) sep_pop_list_ge_2(COMMA, p)
    list(semi) RPAREN {PBin (List.length $2, $3, List.length $4)}
  | LPAREN nonempty_list(semi) separated_nonempty_list(COMMA, p)
    list(semi) RPAREN {PBin (List.length $2, $3, List.length $4)}
  | LPAREN list(semi) separated_nonempty_list(COMMA, p)
    nonempty_list(list) RPAREN {PBin (List.length $2, $3, List.length $4)}
  
  | CAP {PData $1}
  | LBRACE sep_pop_list_ge_2(COMMA, e) RBRACE {$2}
  | LBRACE p RBRACE {PCapture $2}

  | LBRACK SYMBOL RBRACK {PSym $2}  // consider condensing
  | LPAREN bop RPAREN {PSym $2}

%inline p_bop: 
  | PLUS {"+"} | TIMES {"*"} | CAT {"&"}
