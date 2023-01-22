%{
  open Batteries
  open Abstract.Syntax

  open Span
%}

%token
  LPAREN RPAREN
  LBRACK RBRACK
  LBRACE RBRACE
  REL EQ PIPE

%token<string> ID VAR
%token<int> STACK_VAR COSTACK_VAR

%%

value_type: _value_type {$1, make $loc}
%inline _value_type: 
  | ID {TId $1}
  | VAR {TVar $1}
  | LBRACK relation RBRACK {TQuote $2}
  | LBRACE relation RBRACE {TList $2}

%inline twin(x): separated_pair(x, REL, x) {$1}
%inline stack_head: nonempty_list(value_type) {$1}
%inline costack_head: 
  separated_nonempty_list(PIPE, pair(STACK_VAR, stack_head)) {$1}

relation: _relation {$1, make $loc}
%inline _relation: 
  | twin(stack_head) {ImplStack $1}
  | twin(costack_head) {ImplCostack $1}
  | twin(pair(COSTACK_VAR, costack_head)) {Expl $1}
