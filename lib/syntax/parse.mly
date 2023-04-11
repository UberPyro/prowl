%{
  open Batteries
  open Syntax

  open Ast
%}

%token
  LET ASSIGN IN EX DOT
  PIPE QUANT_MARK QUANT_PLUS QUANT_STAR TILDE
  PICK PONDER FORK PAR END
  PICK_OP PONDER_OP FORK_OP PAR_OP
  PLUS MINUS STAR QMARK

  LPAREN RPAREN
  LBRACK RBRACK
  LBRACE RBRACE

  COMMA SEMICOLON BAR

%token<string> STR ID UVAR
%token<int> INT

%right BAR
%left PIPE
%right PICK_OP PONDER_OP
%right FORK_OP PAR_OP

%left PLUS MINUS
%left TIMES

%nonassoc TILDE QUANT_MARK QUANT_PLUS QUANT_STAR QMARK

%%

sect: _sect {$1, $loc}
_sect: 
  | PLUS expr {`add $2}
  | expr PLUS {`add $1}
  | MINUS expr {`subl $2}
  | expr MINUS {`subr $1}
  | STAR expr {`mul $2}
  | expr STAR {`mul $1}

  | PLUS {`sect "+"}
  | MINUS {`sect "-"}
  | STAR {`sect "*"}

e1: _e1 {$1, $loc}
_e1: 
  | e1 BAR e1 {`arrow ($1, $3)}
  | e1 PIPE e1 {`dis ($1, $3)}
  | sep2(PICK_OP, e1) {`pick (List.rev $1)}
  | sep2(PONDER_OP, e1) {`ponder (List.rev $1)}
  | sep2(FORK_OP, e1) {`fork (List.rev $1)}
  | sep2(PAR_OP, e1) {`par (List.rev $1)}
  
  | e1 PLUS e1 {`binop ($1, "+", $3)}
  | e1 MINUS e1 {`binop ($1, "-", $3)}
  | e1 STAR e1 {`binop ($1, "*", $3)}

  | e1 TILDE {`dag $1}
  | e1 QUANT_MARK {`mark $1}
  | e1 QUANT_PLUS {`plus $1}
  | e1 QUANT_STAR {`star $1}
  | e1 QMARK {`isthis $1}

  | LPAREN e1 RPAREN {fst $2}
  | LBRACK e1 RBRACK {quote $2}
  | LBRACE separated_list(COMMA, e1) RBRACE {`list $2}
  | PICK separated_list(SEMICOLON, e1) END {`pick $2}
  | PONDER separated_list(SEMICOLON, e1) END {`ponder $2}
  | FORK separated_list(SEMICOLON, e1) END {`fork $2}
  | PAR separated_list(SEMICOLON, e1) END {`par $2}

  | LET nonempty_list(s1) IN e1 {`bind_var ($2, $4)}
  | EX nonempty_list(ID) DOT e1 {`ex ($2, $4)}

  | INT {`int $1}
  | STR {`str $1}
  | ID {`id $1}
  | UVAR {`uvar $1}

s1: 
  | ID ASSIGN e1 {$1, $3}


sep2(sep, x): 
  x sep separated_nonempty_list(sep, x) {$1 :: $3}
