open! Batteries

open Metadata

type expr = _expr * Span.t
and _expr = 
  | Int of int
  | Str of string
  | Id of string
  | UVar of string

  | Gen | Fab | Elim | Exch
  | Swap | Unit | Call | Zap | Dup | Cat | Dip
  
  | Eq | Cmp
  | Mk | Cnt
  | Write | Read  (* string <-> list *)
  | Parse | Show  (* string <-> int *)

  | Let of stmt list * expr
  | Ex of string list * expr
  | List of expr list

  | Uop of uop
  | Bop of bop

and uop = 
  | Mark | Plus | Star | Dag | Quote | Lift
  | Add | SubL | SubR | Mul

and bop = 
  | Jux | Dis
  | Pick | Ponder
  | Fork | Tensor
  | Copick | Cofork

and stmt = _stmt * Span.t
and _stmt = 
  | Def of string * expr
  [@@deriving show]
