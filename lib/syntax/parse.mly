%{
  open! Batteries
  open! Ast
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

  COMMA SEMICOLON BAR EOF

%token<string> STR ID UVAR
%token<int> INT

%right IN
%right DOT

%right BAR
%left PIPE
%right PICK_OP PONDER_OP
%right FORK_OP PAR_OP

%left PLUS MINUS
%left STAR

%nonassoc TILDE QUANT_MARK QUANT_PLUS QUANT_STAR QMARK

%start<expr> prog

%%

prog: e1 EOF {$1}

sect: _sect {$1, $loc}
_sect: 
  | PLUS e1 {`add $2}
  | e1 PLUS {`add $1}
  | MINUS e1 {`subl $2}
  | e1 MINUS {`subr $1}
  | STAR e1 {`mul $2}
  | e1 STAR {`mul $1}

  | PLUS {`sect "+"}
  | MINUS {`sect "-"}
  | STAR {`sect "*"}
  | e1 {fst $1}

e1: _e1 {$1, $loc}
_e1: 
  | e1 BAR e1 {`arrow ($1, $3)}
  | e1 PIPE e1 {`dis ($1, $3)}
  | e1 PICK_OP e1 {`pick [$3; $1]}
  | e1 PONDER_OP e1 {`ponder [$3; $1]}
  | e1 FORK_OP e1 {`fork [$3; $1]}
  | e1 PAR_OP e1 {`par [$3; $1]}
  
  | e1 PLUS e1 {`binop ($1, "+", $3)}
  | e1 MINUS e1 {`binop ($1, "-", $3)}
  | e1 STAR e1 {`binop ($1, "*", $3)}

  | e1 TILDE {`dag $1}
  | e1 QUANT_MARK {`mark $1}
  | e1 QUANT_PLUS {`plus $1}
  | e1 QUANT_STAR {`star $1}
  | e1 QMARK {`isthis $1}

  | LPAREN sect RPAREN {fst $2}
  | LBRACK sect RBRACK {`quote $2}
  | LBRACE separated_list(COMMA, sect) RBRACE {`list $2}
  | PICK separated_list(SEMICOLON, sect) END {`pick $2}
  | PONDER separated_list(SEMICOLON, sect) END {`ponder $2}
  | FORK separated_list(SEMICOLON, sect) END {`fork $2}
  | PAR separated_list(SEMICOLON, sect) END {`par $2}

  | LET nonempty_list(s1) IN e1 {`bind_var ($2, $4)}
  | EX nonempty_list(ID) DOT e1 {`ex ($2, $4)}

  | INT {`int $1}
  | STR {`str $1}
  | ID {`id $1}
  | UVAR {`uvar $1}

s1: 
  | ID ASSIGN e1 {$1, $3}
