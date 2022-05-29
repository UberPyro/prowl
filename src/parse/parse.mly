%{
  open Batteries
  open Ast

  open Util
  open Parse_proc
  
  let e_t_of_lst : e list -> e_t = function
    | [h, _] -> h
    | lst -> Cat lst
%}

%token
  DEF OPEN MIX IMPL SIG END DO
  OPAQ TYPE DATA SPEC PRIV MOD
  TRY ONE CUT SCORE MANY

  PLUS MINUS TIMES DIVIDE
  EXP RANGE SNOC CONS
  APPEND BIND FISH
  ALT ALT_REL ALT_CUT CAT

  EQ NEQ LT LE GT GE

  COLON TILDE ASSIGN ARROW
  WIDE_ARROW BACKARROW
  COMMA DOT INTERSECT

  BLANK EOF EFFECT

  LPAREN RPAREN
  LBRACK RBRACK
  LBRACE

  IMPL_LBRACK IMPL_RBRACK
  NONCAP_BRACK ATOM_BRACK
  INV_BRACK TIMES_BRACK
  UNIT VOID

%token<string>
  LET AND AS STR
  ID CAP SYMBOL INFIX

%token<int> INT
%token<float> FLOAT
%token<char> CHAR

%token<Ast.e_t> COMB
%token<Ast.quant * Ast.greed> QUANT
%token<Ast.greed> SEMICOLON RBRACE

%left CAT
%left ALT ALT_REL ALT_CUT
%left INTERSECT
%left BIND
%left FISH
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
  | PRIV {Priv}
  | OPAQ {Opaq}
  | {Pub}

sp: sp_t {$1, $loc}
%inline sp_t: 
  | SPEC ID ASSIGN ty {SDef ($2, $4)}
  | TYPE ID list(CAP) ioption(preceded(ASSIGN, ty)) {STy ($2, $3, $4)}
  | DATA ID list(CAP) ASSIGN data {SData ($2, $3, $5)}

%inline data: data_t {$1, $loc}
%inline data_t: 
  | separated_nonempty_list(semi, constructor) {let+ x = $1 in [x]}
  | LBRACE separated_nonempty_list(
    semi, 
    sep_pop_list_ge_2(COMMA, constructor)
  ) rbrace {$2}

%inline constructor: 
  | nonempty_list(ty_term) {
    match List.rev $1 with    
    | (TGen s, _) :: t -> List.rev t, s
    | _ -> failwith "Rightmost term is not a constructor"
  }

ty: ty_t {$1, $loc}
ty_t: 
  | ioption(ty_stack) EFFECT ioption(ty_stack) {
    let ty_conv = function
    | Some st -> st
    | None -> TCat [], $loc in
    ty_conv $1, 
    ty_conv $3
  }
  | ty_stack {(TCat [], $loc), $1}

%inline ty_stack: 
  | ty_term {$1}
  | pop_list_ge_2(ty_term) {TCat $1, $loc}

ty_term: ty_term_t {$1, $loc}
%inline ty_term_t: 
  | ID {TId $1}
  | CAP {TGen $1}
  | ty_term DOT ID {TAccess ($1, $3)}
  | LBRACE rbrace {TCapture (((TCat [], $loc), (TCat [], $loc)), $loc)}
  | LBRACE ty rbrace {TCapture $2}
  | LBRACK ty RBRACK {TList $2}
  | LBRACK ty_stack WIDE_ARROW ty RBRACK {TMap ($2, $4)}
  | UNIT {TUnit}
  | VOID {TVoid}
  | LPAREN separated_nonempty_list(
    semi, 
    separated_nonempty_list(COMMA, ty)
  ) RPAREN {TBin $2}
  | MOD list(sp) END {TMod $2}
  | SIG list(sp) END {TSig $2}
  | LT ID COLON ty GT {TImpl ($2, $4)}

%inline pop_list_ge_2(entry): 
  | entry nonempty_list(entry) {$1 :: $2}

%inline sep_pop_list_ge_2(sep, entry): 
  | entry sep separated_nonempty_list(sep, entry) {$1 :: $3}

s: s_t {$1, $loc}
%inline s_t: 
  | access s_kw p ioption(preceded(COLON, ty)) ASSIGN e
    {Def ($1, $2, $3, $6, $4)}
  | OPEN is_impl e {Open ($2, $3)}
  | MIX e {Mix $2}
  | access TYPE ID list(CAP) ioption(preceded(ASSIGN, ty))
    {Ty ($1, $3, $4, $5)}
  | access DATA ID list(CAP) ASSIGN data
    {Data ($1, $3, $4, $6)}

%inline s_kw: 
  | DEF {false}
  | DEF IMPL {true}

%inline is_impl: 
  | IMPL {true}
  | {false}

%inline det_control:
  | ioption(TRY) det_control_t {Det ($1 <> None, $2)}
%inline det_control_t: 
  | ONE {DOne}
  | CUT {DCut}
  | SCORE {DScore}
  | MANY {DMany}

e: e_t {$1, $loc}
%inline e_t: 
  | bop {Sect $1}
  | bop bexp {SectLeft ($1, $2)}
  | bexp bop {SectRight ($1, $2)}
  | bexp {let b, _ = $1 in b}
  | bind_e {let b, _ = $1 in b}
  | bexp bind_e {Cat [$1; $2]}
  | bexp bop bind_e {Bop ($1, $2, $3)}

%inline bind_e: bind_e_t {$1, $loc}
%inline bind_e_t: 
  | let_body(LET) list(let_body(AND)) ARROW e {Let ($1 :: $2, $4)}
  | AS p ARROW e {As ($1, $2, $4)}

%inline let_body(kw): 
  | kw is_impl p ASSIGN e {$1, $2, $3, $5}

bexp: bexp_t {$1, $loc}
%inline bexp_t: 
  | bexp bop bexp {Bop ($1, $2, $3)}
  | nonempty_list(term) {e_t_of_lst $1}

term: term_t {$1, $loc}
%inline term_t: 
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
  | DO e END {let (d, _) = $2 in d}
  | LPAREN semi RPAREN {Case []}
  | LBRACE rbrace {Capture (Cat [], $loc)}
  | LBRACE semi rbrace {Capture (Case [], $loc)}
  | LBRACK e nonempty_list(pair(SEMICOLON, e)) RBRACK {Case ((Gre, $2) :: $3)}
  | INV_BRACK separated_list(semi, e) RBRACK {Inv $2}

  | LBRACK separated_list(COMMA, e) RBRACK {List $2}
  | LBRACK separated_nonempty_list(
    COMMA, 
    separated_pair(e, WIDE_ARROW, e)
  ) RBRACK {Map $2}
  | bin(e) {
    match $1 with
    | 0, [x], 0 -> Cap x
    | l, lst, r -> proc_ebin (l, lst, r) |> fst
  }
  
  | CAP {EData $1}
  | LBRACE sep_pop_list_ge_2(COMMA, e) rbrace {Prod $2}
  | MOD list(s) END {Mod $2}
  | IMPL_LBRACK e IMPL_RBRACK {Impl $2}
  | LBRACE e rbrace {Capture $2}
  | det_control {$1}

  | SYMBOL {Id $1}
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
  | APPEND {"++"} | BIND {">>="} | FISH {">=>"}
  | ALT {"|"} | ALT_REL {"|?"} | ALT_CUT {"|+"} | CAT {"&"} | INTERSECT {"&&"}
  | INFIX {$1}

%inline semi: SEMICOLON {assert ($1 == Gre); ";"}
%inline rbrace: RBRACE {assert ($1 == Gre)}

bin(entry): 
  | LPAREN list(semi) separated_nonempty_list(COMMA, entry) list(semi) RPAREN
    {List.length $2, $3, List.length $4}

p: p_t {$1, $loc}
%inline p_t: 
  | p p_bop p {PBop ($1, $2, $3)}
  | nonempty_list(p_term) {
    match $1 with
    | [h, _] -> h
    | lst -> PCat lst
  }

p_term: p_term_t {$1, $loc}
%inline p_term_t: 
  | ID {PId $1}
  | p_term DOT ID {PAccess ($1, $3)}
  | COMB {
    match $1 with
    | StackComb [Zap _, _]  -> PBlank
    | _ -> failwith "Stack Combinator in Pattern"
  }
  | BLANK {PBlank}
  | OPEN is_impl {POpen $2}

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
    | 0, [x, _], 0 -> x
    | l, lst, r -> proc_pbin (l, lst, r) |> fst
  }
  
  | CAP {PData $1}
  | LBRACE sep_pop_list_ge_2(COMMA, p) RBRACE {PProd $2}
  | LPAREN list(semi) p COLON ty RPAREN
    {assert (List.length $2 == 0); PAsc ($3, $5)}
  | LT p COLON ty GT {PImpl ($2, $4)}
  | LBRACE p RBRACE {PCapture $2}

  | LBRACK SYMBOL RBRACK {PId $2}  // consider condensing
  | LPAREN list(semi) ext_name RPAREN {PId $3}

%inline p_bop: 
  | PLUS {"+"} | TIMES {"*"} | CAT {"&"}
  | CONS {"-<"} | SNOC {">-"}

%inline ext_name: 
  | bop {$1}
  | LET {"let" ^ $1}
  | AND {"and" ^ $1}
  | AS {"as" ^ $1}
