%{
  open Batteries

  open Ast
%}

%token<int> DUP ZAP ROT RUN
%token EOF

%start<e_t> parse

%%

%inline op: op_t {$1, $loc}
%inline op_t: 
  | DUP {Dup $1}
  | ZAP {Zap $1}
  | ROT {Rot $1}
  | RUN {Run $1}

parse: nonempty_list(op) EOF {StackComb $1}
