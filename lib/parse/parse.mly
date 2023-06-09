%{
  open! Batteries
  open! Metadata
  open! Ast
  open! Types
%}

%token
  LPAREN RPAREN
  LBRACK RBRACK
  LET ASSIGN IN
  ADD SUB MUL
  EQ NEQ GT LT GE LE
  DAG MARK PLUS STAR
  TENSOR PONDER FORK PICK CROSS GUESS
  CONTRA UNION
  GEN FAB EXCH ELIM CMP
  DUP ZAP SWAP CONS DIP CAT UNIT
  DIVMOD LIN BIN PARSE SHOW
  NOP ID AB
  COMMA EOF

%token<string> VAR STRING
%token<int> INT

// %left UNION
// %left PONDER PICK GUESS
// %left TENSOR FORK CROSS
// %left EQ NEQ GT LT GE LE
// %left ADD SUB MUL

%start<stmt list> prog

%%

prog: list(stmt) EOF {$1}

stmt: _stmt {$1, $loc}
%inline _stmt: 
  | ASSIGN VAR uexpr {Def ($2, $3)}

sect: _sect {$1, $loc, mk_dc (), mk_dc ()}
%inline _sect: 
  | bop uexpr {SectLeft ($1, $2)}
  | uexpr bop {SectRight ($1, $2)}
  | bop {Sect $1}
  | _uexpr {$1}

uexpr: _uexpr {$1, $loc, mk_dc (), mk_dc ()}
%inline _uexpr: 
  | juxt DAG {Uop ($1, Dag)}
  | juxt MARK {Uop ($1, Mark)}
  | juxt PLUS {Uop ($1, Plus)}
  | juxt STAR {Uop ($1, Star)}
  | _juxt {$1}

juxt: _juxt {$1, $loc, mk_dc (), mk_dc ()}
%inline _juxt: 
  // | juxt CONTRA expr {Dop ($1, Contra, $3)}
  // | juxt expr {Dop ($1, Jux, $2)}
  | _expr {$1}

expr: _expr {$1, $loc, mk_dc (), mk_dc ()}
%inline _expr: 
  | term bop expr {Bop ($1, $2, $3)}
  | term dop expr {Dop ($1, $2, $3)}
  | _term {$1}

term: _term {$1, $loc, mk_dc (), mk_dc ()}
%inline _term: 
  // | lit {Lit $1}
  // | LPAREN sect RPAREN {Tuple4.first $2}
  | LPAREN RPAREN {Nop Noop}
  // | LET list(stmt) IN uexpr {Let ($2, $4)}
  | nop {Nop $1}
  | VAR {Var $1}

%inline lit: 
  | INT {Int $1}
  | STRING {String $1}
  | LBRACK sect RBRACK {Quote $2}
  | LPAREN COMMA RPAREN {List []}
  | LPAREN sect COMMA RPAREN {List [$2]}
  | LPAREN sect COMMA separated_nonempty_list(COMMA, sect) RPAREN
    {List ($2 :: $4)}

%inline bop: 
  | ADD {Aop Add}
  | SUB {Aop Sub}
  | MUL {Aop Mul}
  | EQ {Cop Eq}
  | NEQ {Cop Neq}
  | LT {Cop Lt}
  | LE {Cop Le}
  | GT {Cop Gt}
  | GE {Cop Ge}

%inline dop: 
  | PONDER {Ponder}
  | TENSOR {Tensor}
  | PICK {Pick}
  | FORK {Fork}
  | GUESS {Guess}
  | CROSS {Cross}
  | UNION {Union}

%inline nop: 
  | GEN {Gen} | FAB {Fab} | EXCH {Exch} | ELIM {Elim} | CMP {Cmp}
  | DUP {Dup} | ZAP {Zap} | SWAP {Swap} | CONS {Cons} | DIP {Dip}
  | CAT {Cat} | UNIT {Unit}
  | DIVMOD {DivMod} | LIN {Lin} | BIN {Bin} | PARSE {Parse} | SHOW {Show}
  | NOP {Noop} | ID {Id} | AB {Ab}
