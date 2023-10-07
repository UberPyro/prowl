%{
  open! Batteries
  open! Metadata
  open! Types
  open! Ast

  let conv (b1, b2) = Uref.uref [[ref (Det.BConst (b1, b2))]]
%}

%token
  LPAREN RPAREN
  LBRACK RBRACK
  LBRACE RBRACE
  LET ASSIGN SPECIFY IN
  EXISTS EACH WITHIN ARROW
  ADD SUB MUL
  EQ NEQ GT LT GE LE
  DAG MARK PLUS STAR
  TENSOR PONDER FORK PICK CROSS GUESS
  UNION INTER SEMI PROCAT PROVERSE RANGE
  GEN FAB EXCH ELIM CMP
  DUP ZAP SWAP CONS DIP CAT UNIT CALL
  DIVMOD PARSE SHOW PUSH ENQ
  NOP ID AB ALT SAN
  EOF
  PIPE BAR DOT DOLLAR
  SPLIT FN PT MT RL XOR AND

%token<string> VAR CAP STRING COSTACK_VAR STACK_VAR
%token<int> INT

%start<stmt list> prog

%left AND
%left XOR

%nonassoc IN WITHIN ARROW
%left PROCAT PROVERSE SEMI
%left UNION
%left INTER
%left PONDER PICK GUESS
%left TENSOR FORK CROSS
%left ALT
%left CAT
%left RANGE
%left GT LT GE LE
%left EQ NEQ
%left ADD SUB MUL

%%

prog: list(stmt) EOF {$1}

det: 
  | FN {DLit (conv (true, true))}
  | PT {DLit (conv (false, true))}
  | MT {DLit (conv (true, false))}
  | RL {DLit (conv (false, false))}
  | CAP {DVar $1}
  | det AND det {DAnd ($1, $3)}
  | det XOR det {DXor ($1, $3)}

%inline mode: 
  | SPECIFY det SPLIT det {$2, $4}
  | { DLit (conv (false, false)), 
      DLit (conv (true, true)) }

ty_expr: 
  | costack_ty BAR costack_ty mode
    {let d1, d2 = $4 in Explicit ($1, $3, d1, d2)}
  | nonempty_list(stack_ty) BAR nonempty_list(stack_ty) mode
    {let d1, d2 = $4 in ImplicitCostack ($1, $3, d1, d2)}
  | list(value_ty) BAR list(value_ty) mode
    {let d1, d2 = $4 in ImplicitStack ($1, $3, d1, d2)}
costack_ty: 
  | COSTACK_VAR list(preceded(PIPE, stack_ty)) {Some $1, $2}
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
  | EXISTS nonempty_list(CAP) ex_binder expr
    {List.fold_left (fun b a -> Ex (a, b, $3), $loc, fresh ()) $4 $2 |> Tuple3.first}
  | expr ex_binder nonempty_list(STACK_VAR) EACH
    {List.fold_left (fun b a -> Each (b, a, $2), $loc, fresh ()) $1 $3 |> Tuple3.first}
  | hiexpr list(hiexpr) {
    let rec go e = function
      | h :: t -> 
        Dop (e, Jux, (go h t, Tuple3.second h, fresh ()))
      | [] -> Tuple3.first e in
    go $1 $2
  }

%inline ex_binder : WITHIN {false} | ARROW {true}

hiexpr: _hiexpr {$1, $loc, fresh ()}
_hiexpr: 
  | hiexpr uop {Uop ($1, $2)}
  | _term {$1}

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
  | CAT {Lop Cat}
  | ALT {Lop Alt}
  | RANGE {Range}

%inline dop: 
  | PONDER {Ponder}
  | TENSOR {Tensor}
  | PICK {Pick}
  | FORK {Fork}
  | GUESS {Guess}
  | CROSS {Cross}

  | UNION {Union}
  | INTER {Inter}
  | PROCAT {Procat}
  | PROVERSE {Proverse}
  | SEMI {Jux}

%inline uop: 
  | DAG {Dag}
  | MARK {Mark}
  | PLUS {Plus}
  | STAR {Star}

%inline nop: 
  | GEN {Gen} | FAB {Fab} | EXCH {Exch} | ELIM {Elim} | CMP {Cmp}
  | DUP {Dup} | ZAP {Zap} | SWAP {Swap} | CONS {Cons} | DIP {Dip}
  | UNIT {Unit} | CALL {Call}
  | DIVMOD {DivMod} | PARSE {Parse} | SHOW {Show}
  | PUSH {Push} | ENQ {Enq}
  | NOP {Noop} | ID {Id} | AB {Ab} | SAN {San}
