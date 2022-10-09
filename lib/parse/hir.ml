type 'a expr = 'a _expr * 'a
and 'a _expr = 
  | Cat of 'a word list
  | Let of bool * string * string list * 'a expr * 'a expr
  | As of string * string list * 'a expr
and 'a word = 'a _word * 'a
and 'a _word = 
  | Int of int
  | Char of char
  | Quote of 'a expr
  | List of 'a expr
  | Id of string

(* module Ast_to_hir = struct
  let rec expr (e, l) = _expr e, l
  and _expr : 'a Ast._expr -> 'a _expr = function
    | Cat ws -> Cat (List.map word ws)
    | Let (b, s, ss, e1, e2) -> Let (b, s, ss, expr e1, expr e2)
    | As (s, ss, e) -> As (s, ss, expr e)

  and word (w, l) = _word w, l
  and _word = function
    | Int i -> Int i
    | Char c -> Char c
    | Quote q -> Quote (expr q)
    | List q -> List (expr q)
    | Id x -> Id x
  
end *)
