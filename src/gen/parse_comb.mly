%{
  open Batteries

  open Ast
%}

%token<int> DUP ZAP ROT RUN
%token EOF

%start<e> parse

%%

%inline op: 
  | DUP {Dup $1}
  | ZAP {Zap $1}
  | ROT {Rot $1}
  | RUN {Run $1}

parse: nonempty_list(op) EOF {StackComb $1}
