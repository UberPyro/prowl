%{
  open! Batteries
  open! Metadata
  open! Types
  open! Ast
%}

%token
  LPAREN RPAREN
  LBRACK RBRACK
  LBRACE RBRACE
  LET ASSIGN IN
  ADD SUB MUL
  EQ NEQ GT LT GE LE
  DAG MARK PLUS STAR
  TENSOR PONDER FORK PICK CROSS GUESS
  CONTRA UNION
  GEN FAB EXCH ELIM CMP
  DUP ZAP SWAP CONS DIP CAT UNIT
  DIVMOD LIN PARSE SHOW
  NOP ID AB
  COMMA EOF

%token<string> VAR STRING
%token<int> INT

%start<stmt list> prog

%nonassoc IN
%left UNION
%left PONDER PICK GUESS
%left TENSOR FORK CROSS
%left GT LT GE LE
%left EQ NEQ
%left ADD SUB MUL

%%

prog: list(stmt) EOF {$1}

stmt: _stmt {$1, $loc}
%inline _stmt: 
  | ASSIGN VAR expr {Def ($2, $3)}

sect: _sect {$1, $loc, fresh ()}
%inline _sect: 
  | bop expr {SectLeft ($1, $2)}
  | expr bop {SectRight ($1, $2)}
  | bop {Sect $1}
  | _expr {$1}

expr: _expr {$1, $loc, fresh ()}
_expr: 
  | expr bop expr {Bop ($1, $2, $3)}
  | expr dop expr {Dop ($1, $2, $3)}
  | LET nonempty_list(stmt) IN expr {Let ($2, $4)}
  | _hiexpr {$1}

hiexpr: _hiexpr {$1, $loc, fresh ()}
_hiexpr: 
  | hiexpr DAG {Uop ($1, Dag)}
  | hiexpr MARK {Uop ($1, Mark)}
  | hiexpr PLUS {Uop ($1, Plus)}
  | hiexpr STAR {Uop ($1, Star)}
  | term list(pair(ioption(CONTRA), term)) {
    let rec go e = function
      | (None, h) :: t -> 
        Dop (e, Jux, (go h t, Tuple3.second h, fresh ()))
      | (Some _, h) :: t -> 
        Dop (e, Contra, (go h t, Tuple3.second h, fresh ()))
      | [] -> Tuple3.first e in
    go $1 $2
  }

term: _term {$1, $loc, fresh ()}
%inline _term: 
  | lit {Lit $1}
  | LPAREN _sect RPAREN {$2}
  | LPAREN RPAREN {Nop Noop}
  | nop {Nop $1}
  | VAR {Var $1}

%inline lit: 
  | INT {Int $1}
  | STRING {String $1}
  | LBRACK sect RBRACK {Quote $2}
  | LBRACE separated_list(COMMA, sect) RBRACE {List $2}

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
  | DIVMOD {DivMod} | LIN {Lin} | PARSE {Parse} | SHOW {Show}
  | NOP {Noop} | ID {Id} | AB {Ab}
