%{
  open Batteries
  open Abstract.Syntax

  open Span
%}

%token
  LPAREN RPAREN
  LBRACK RBRACK
  LBRACE RBRACE
  REL EQ
  DEF SPEC TYPE DATA

%token<string> ID VAR
%token<int> STACK_VAR COSTACK_VAR

%%

value_type: _value_type {$1, make $loc}
%inline _value_type: 
  | ID {TId $1}
  | VAR {TVar $1}
  | LBRACK relation RBRACK {TQuote $2}
  | LBRACE relation RBRACE {TList $2}


