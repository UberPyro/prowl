%{
  open Batteries

  open Ast
%}

%token<int> DUP ZAP ROT RUN

%start<e> comb

%%

%inline op: 
  | DUP {Dup $1}
  | ZAP {Zap $1}
  | ROT {Rot $1}
  | RUN {Run $1}

comb: nonempty_list(op) {Comb $1}
