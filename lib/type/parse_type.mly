%{
  open Batteries
  open Type
  
  module Dict = Map.Make(struct
    type t = string
    let compare = compare
  end)
%}

%token
  EOF SP HASTYPE ALT
  LBRACK LBRACE
  RBRACK RBRACE

%token<string>
  COSTACK STACK VAR MONO

%left ALT

%start<Type.t>

%% 

/* need memoized function to identify identical fresh args */

// stack: STACK list(VAR) {}
