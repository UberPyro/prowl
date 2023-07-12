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
  LET ASSIGN SPECIFY IN EXISTS
  ADD SUB MUL
  EQ NEQ GT LT GE LE
  DAG MARK PLUS STAR APPLY INDUCE
  TENSOR PONDER FORK PICK CROSS GUESS
  CONTRA UNION
  GEN FAB EXCH ELIM CMP SURF
  DUP ZAP SWAP CONS DIP CAT UNIT DIG
  DIVMOD LIN PARSE SHOW
  NOP ID AB AP APL APR
  COMMA EOF
  PIPE BAR DOT DOLLAR

%token<string> VAR CAP STRING COSTACK_VAR STACK_VAR
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

ty_expr: 
  | costack_ty BAR costack_ty {Explicit ($1, $3)}
  | nonempty_list(stack_ty) BAR nonempty_list(stack_ty) {ImplicitCostack ($1, $3)}
  | list(value_ty) BAR list(value_ty) {ImplicitStack ($1, $3)}
costack_ty: 
  | COSTACK_VAR separated_list(PIPE, stack_ty) {Some $1, $2}
  | DOLLAR separated_list(PIPE, stack_ty) {None, $2}
stack_ty: 
  | STACK_VAR list(value_ty) {Some $1, $2}
  | DOT list(value_ty) {None, $2}
%inline value_ty: 
  | VAR {match $1 with
    | "z" -> TyInt
    | "str" -> TyString
    | _ -> failwith @@ Printf.sprintf "Unrecognized type %s" $1
  }
  | LBRACK ty_expr RBRACK {TyQuote $2}
  | LBRACE ty_expr RBRACE {TyList $2}
  | CAP {TyVal $1}

stmt: _stmt {$1, $loc}
%inline _stmt: 
  | ASSIGN VAR expr {Def ($2, None, $3)}
  | SPECIFY VAR ty_expr ASSIGN VAR expr {
    if $2 <> $5 then failwith @@ Printf.sprintf
      "Mismatched names: definition [%s] and annotation [%s]" $2 $5
    else Def ($5, Some $3, $6)
  }

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
  | EXISTS nonempty_list(CAP) IN expr
    {List.fold_right (fun a b -> Ex (a, b), $loc, fresh ()) $2 $4 |> Tuple3.first}
  | EXISTS nonempty_list(typed_ex) IN expr {TypedEx ($2, $4)}
  | hiexpr list(pair(ioption(CONTRA), hiexpr)) {
    let rec go e = function
      | (None, h) :: t -> 
        Dop (e, Jux, (go h t, Tuple3.second h, fresh ()))
      | (Some _, h) :: t -> 
        Dop (e, Contra, (go h t, Tuple3.second h, fresh ()))
      | [] -> Tuple3.first e in
    go $1 $2
  }

%inline typed_ex: SPECIFY CAP ty_expr {$2, $3}

hiexpr: _hiexpr {$1, $loc, fresh ()}
_hiexpr: 
  | hiexpr DAG {Uop ($1, Dag)}
  | hiexpr MARK {Uop ($1, Mark)}
  | hiexpr PLUS {Uop ($1, Plus)}
  | hiexpr STAR {Uop ($1, Star)}
  | hiexpr APPLY {Uop ($1, Apply)}
  | hiexpr INDUCE {Uop ($1, Induce)}
  | _term {$1}

%inline _term: 
  | lit {Lit $1}
  | LPAREN _sect RPAREN {$2}
  | LPAREN RPAREN {Nop Noop}
  | nop {Nop $1}
  | VAR {Var $1}
  | CAP {UVar $1}

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
  | GEN {Gen} | FAB {Fab} | EXCH {Exch} | ELIM {Elim} | CMP {Cmp} | SURF {Surf}
  | DUP {Dup} | ZAP {Zap} | SWAP {Swap} | CONS {Cons} | DIP {Dip}
  | CAT {Cat} | UNIT {Unit} | DIG {Dig}
  | DIVMOD {DivMod} | LIN {Lin} | PARSE {Parse} | SHOW {Show}
  | NOP {Noop} | ID {Id} | AB {Ab}
  | AP {Ap} | APL {ApL} | APR {ApR}
