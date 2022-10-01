%{
  open Batteries
  open Ast
  open ContentAst
  open Parse_proc
%}

%token
  EOF FN ASSIGN ALT COMMA
  LPAREN LBRACK LBRACE
  RPAREN RBRACK RBRACE

%token<string>
  STR ID
  SLOW AP PLUS
  TIMES EXP POST

%token<int> INT
%token<char> CHAR

%left SLOW
%left AP
%left PLUS
%left TIMES

%nonassoc POST
%left EXP

%start<ContentAst.p> program

%%

program: list(s) EOF {$1}

s: _s {create $1 $loc}
%inline _s: FN ID ASSIGN e {$2, $4}

e: _e {create $1 $loc}
%inline _e: separated_nonempty_list(ALT, term) {$1}

term: 
  | term binop term {create_term [
    QuoteLit (e_of_ws $1 $loc); 
    QuoteLit (e_of_ws $3 $loc); 
    $2; 
    Id "call"; 
  ] $loc}
  | nonempty_list(unit) {$1}

%inline binop: 
  | SLOW {Id $1}
  | AP {Id $1}
  | PLUS {Id $1}
  | TIMES {Id $1}

unit: _unit {create $1 $loc}
%inline _unit: 
  | unit POST {create_seq [
    QuoteLit (e_of_w $1 $loc); 
    Id $2; 
    Id "call"; 
  ] $loc}
  | unit EXP unit {create_seq [
    QuoteLit (e_of_w $1 $loc); 
    QuoteLit (e_of_w $3 $loc); 
    Id $2; 
    Id "call"; 
  ] $loc}
  | _w {$1}

%inline _w: 
  | INT {IntLit $1}
  | CHAR {CharLit $1}
  | STR {ListLit (List.map
    (fun x -> create [[create (CharLit x) $loc]] $loc)
    (String.to_list $1)
  )}
  | LBRACK RBRACK {QuoteLit (create [[]] $loc)}
  | LBRACK e RBRACK {QuoteLit $2}
  | LBRACE separated_list(COMMA, e) RBRACE {ListLit $2}
  | ID {Id $1}
  | LPAREN RPAREN {Expr (create [[]] $loc)}
  | LPAREN e RPAREN {Expr $2}
  // | LPAREN e ext_binop RPAREN {create_seq [
  //   Id "mono"; 
  //   QuoteLit $2; 
  //   $3; 
  //   Id "call"; 
  // ] $loc}
  // | LPAREN ext_binop e RPAREN {create_seq [
  //   Id "mono"; 
  //   QuoteLit $3; 
  //   Id "swap"; 
  //   $2; 
  //   Id "call"; 
  // ] $loc}
  | LPAREN op RPAREN {$2}

%inline ext_binop: 
  | binop {$1}
  | EXP {Id $1}

%inline op: 
  | ext_binop {$1}
  | POST {Id $1}
