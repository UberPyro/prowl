open Batteries

open Ast
open ContentAst

let e_of_w w loc : e = 
  create [[w]] loc

let e_of_ws ws loc : e = 
  create [ws] loc

let create_term seq loc = 
  List.map (flip create loc) seq

let create_seq seq loc = 
  let c = flip create loc in 
  Expr (create [List.map c seq] loc)
