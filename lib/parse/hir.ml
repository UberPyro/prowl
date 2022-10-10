open Batteries

type 'a expr = 'a _expr * 'a
and 'a _expr = 
  | Cat of 'a word list
  | Let of bool * string * string list * 'a expr * 'a expr
  | As of string list * 'a expr
and 'a word = 'a _word * 'a
and 'a _word = 
  | Int of int
  | Char of char
  | Quote of 'a expr
  | List of 'a expr list
  | Id of string

module Ast_to_hir = struct
  let rec expr (e, l) = _expr e, l
  and _expr : 'a Ast._expr -> 'a _expr = function
    | Cat ws -> Cat (List.map word ws |> List.flatten)
    | Let (b, s, ss, e1, e2) -> Let (b, s, ss, expr e1, expr e2)
    | As (ss, e) -> As (ss, expr e)
    | Bop (e1, bop, e2) -> Cat [
      Quote (expr e1), snd e1; 
      Quote (expr e2), snd e2; 
      Id bop, snd e2; 
    ]
    | Uop (e1, uop) -> Cat [
      Quote (expr e1), snd e1; 
      Id uop, snd e1; 
    ]

  and word (w, l) = List.map (fun x -> x, l) (_word w)
  and _word = function
    | Int i -> [Int i]
    | Char c -> [Char c]
    | Quote q -> [Quote (expr q)]
    | List qs -> [List (List.map expr qs)]
    | Id x -> [Id x]
    | SectLeft (x, q) -> [Quote (expr q); Id x; Id "call"]
    | SectRight (q, x) -> [Quote (expr q); Id "swap"; Id x; Id "call"]
end
