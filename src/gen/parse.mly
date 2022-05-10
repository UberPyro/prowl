%{
  open Batteries
  open Ast

  open Util

  let ty_conv = function
    | Some st -> st
    | None -> TCat []
%}

%token
  DEF OPEN MIX USE IMPL SIG
  PUB OPAQ TYPE ALIAS CLASS
  DATA

  PLUS MINUS TIMES DIVIDE
  EXP RANGE SNOC CONS
  APPEND BIND KLEISLI
  ALT CAT

  EQ NEQ LT LE GT GE

  COLON TILDE ASSIGN ARROW
  WIDE_ARROW BACKARROW
  COMMA DOT INTERSECT

  USCORE BLANK EOF
  CONSTRAINT EFFECT

  LPAREN RPAREN
  LBRACK RBRACK
  LBRACE

  NONCAP_BRACK ATOM_BRACK
  INV_BRACK TIMES_BRACK
  DO MOD END UNIT VOID

%token<string>
  LET AND AS STR
  ID CAP SYMBOL INFIX

%token<int> INT
%token<float> FLOAT
%token<char> CHAR

%token<Ast.e> COMB
%token<Ast.quant * Ast.greed> QUANT
%token<Ast.greed> SEMICOLON RBRACE

%left CAT
%left ALT
%left INTERSECT
%left BIND
%left KLEISLI
%left EQ NEQ
%left LT GT LE GE
%left RANGE
%left SNOC
%left APPEND
%right CONS
%left INFIX
%left PLUS MINUS
%left TIMES DIVIDE
%right EXP
%nonassoc TIMES_BRACK QUANT
%left TILDE
%left DOT

%start<Ast.program> program

%%

program: access e EOF {$1, $2}

%inline access: 
  | PUB {Pub}
  | OPAQ {Opaq}
  | {Priv}

sp: 
  | DEF ID ASSIGN ty {SDef ($2, $4)}
  | TYPE ID ioption(separated_pair(list(CAP), ASSIGN, ty)) {STy ($2, $3)}
  | DATA ID list(CAP) ASSIGN data {SData ($2, $3, $5)}

%inline ty: ioption(constr) ty_eff {Option.default (TCat []) $1, $2}
%inline constr: ty_eff CONSTRAINT {
  match $1 with
  | (TCat [], v) -> v
  | (TCat _, _) -> failwith "Constraints cannot have an input"
  | _ -> failwith "Not a constraint"
}

%inline data: ioption(constr) data_term {Option.default (TCat []) $1, $2}
%inline data_term: 
  | separated_nonempty_list(semi, constructor) {let+ x = $1 in [x]}
  | LBRACE separated_nonempty_list(
    semi, 
    sep_pop_list_ge_2(COMMA, constructor)
  ) rbrace {$2}

%inline constructor: 
  | nonempty_list(ty_term) {
    match List.rev $1 with    
    | (TGen s) :: t -> List.rev t, s
    | _ -> failwith "Rightmost term is not a constructor"
  }

ty_eff: 
  | ioption(ty_stack) EFFECT ioption(ty_stack)
    {ty_conv $1, ty_conv $3}
  | ty_stack {TCat [], $1}

%inline ty_stack: 
  | ty_term {$1}
  | pop_list_ge_2(ty_term) {TCat $1}

ty_term: 
  | ID {TId $1}
  | CAP {TGen $1}
  | ty_term DOT ID {TAccess ($1, $3)}
  | LBRACE rbrace {TCapture (TCat [], TCat [])}
  | LBRACE ty_eff rbrace {TCapture $2}
  | LBRACK ty_eff RBRACK {TList $2}
  | LBRACK ty_stack WIDE_ARROW ty_eff RBRACK {TMap ($2, $4)}
  | UNIT {TUnit}
  | VOID {TVoid}
  | LPAREN separated_nonempty_list(
    semi, 
    separated_nonempty_list(COMMA, ty_eff)
  ) RPAREN {TBin $2}
  | MOD list(sp) END {TMod $2}
  | SIG list(sp) END {TSig $2}

%inline pop_list_ge_2(entry): 
  | entry nonempty_list(entry) {$1 :: $2}

%inline sep_pop_list_ge_2(sep, entry): 
  | entry sep separated_nonempty_list(sep, entry) {$1 :: $3}

s: 
  | access s_kw p ioption(preceded(COLON, ty)) ASSIGN e
    {Def ($1, $2, $3, $6, $4)}
  | OPEN e {Open $2}
  | USE e {Use $2}
  | ioption(IMPL) MIX e {Mix (Option.map (fun _ -> `impl) $1, $3)}
  | access ty_kw ID ioption(separated_pair(list(CAP), ASSIGN, ty))
    {Ty ($1, $2, $3, $4)}
  | access data_kw ID list(CAP) ASSIGN data
    {Data ($1, $2, $3, $4, $6)}

%inline s_kw: 
  | DEF {`def}
  | IMPL {`impl}

%inline ty_kw: 
  | TYPE {`ty}
  | CLASS {`class_}

%inline data_kw: 
  | DATA {`data}
  | ALIAS {`alias}

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
  | bexp bop bexp {Bop ($1, $2, $3)}
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
  | DO e END {$2}
  | LPAREN semi RPAREN {Inv []}
  | LBRACK e nonempty_list(pair(SEMICOLON, e)) RBRACK {Case ($2, $3)}
  | INV_BRACK separated_list(semi, e) RBRACK {Inv $2}

  | LBRACK separated_list(COMMA, e) RBRACK {List $2}
  | LBRACK separated_nonempty_list(
    COMMA, 
    separated_pair(e, WIDE_ARROW, e)
  ) RBRACK {Map $2}
  | bin(e) {
    match $1 with
    | 0, [x], 0 -> Cap x
    | l, lst, r -> Bin (l, lst, r)
  }
  
  | CAP {EData $1}
  | LBRACE sep_pop_list_ge_2(COMMA, e) rbrace {Prod $2}
  | MOD list(s) END {Mod $2}
  | LBRACE e rbrace {Capture $2}

  | SYMBOL {Sym $1}
  | COMB {$1}
  | term QUANT {Quant ($1, fst $2, snd $2)}
  | term TIMES_BRACK e RBRACE {Quant ($1, Num $3, $4)}
  | term TIMES_BRACK e COMMA RBRACE {Quant ($1, Min $3, $5)}
  | term TIMES_BRACK COMMA e RBRACE {Quant ($1, Max $4, $5)}
  | term TIMES_BRACK e COMMA e RBRACE {Quant ($1, Range ($3, $5), $6)}

  | NONCAP_BRACK e RPAREN {Noncap $2}
  | ATOM_BRACK e RPAREN {Atomic $2}

%inline bop: 
  | PLUS {"+"} | MINUS {"-"} | TIMES {"*"} | DIVIDE {"/"} 
  | EXP {"**"} | RANGE {".."} | SNOC {">-"} | CONS {"-<"}
  | GT {">"} | GE {">="} | LT {"<"} | LE {"<="} | EQ {"=="} | NEQ {"/="}
  | APPEND {"++"} | BIND {">>="} | KLEISLI {">=>"}
  | ALT {"|"} | CAT {"&"} | INTERSECT {"&&"} | INFIX {$1}

%inline semi: SEMICOLON {assert ($1 == Gre); ";"}
%inline rbrace: RBRACE {assert ($1 == Gre)}

bin(entry): 
  | LPAREN list(semi) separated_nonempty_list(COMMA, entry) list(semi) RPAREN
    {List.length $2, $3, List.length $4}

p: 
  | p p_bop p {PBop ($1, $2, $3)}
  | LPAREN list(semi) p COLON ty RPAREN
    {assert (List.length $2 == 0); PAsc ($3, $5)}
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
  | UNIT {PUnit}
  
  | LBRACK separated_list(COMMA, p) RBRACK {PList $2}
  | LBRACK separated_nonempty_list(
    COMMA, 
    separated_pair(p, BACKARROW, e)
  ) RBRACK {PMap (let+ x, y = $2 in y, x)}
  | bin(p) {
    match $1 with
    | 0, [x], 0 -> x
    | l, lst, r -> PBin (l, lst, r)
  }
  
  | CAP {PData $1}
  | LBRACE sep_pop_list_ge_2(COMMA, p) RBRACE {PProd $2}
  | LBRACE p RBRACE {PCapture $2}

  | LBRACK SYMBOL RBRACK {PSym $2}  // consider condensing
  | LPAREN bop RPAREN {PSym $2}

%inline p_bop: 
  | PLUS {"+"} | TIMES {"*"} | CAT {"&"}
