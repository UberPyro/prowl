open Metadata
open Typerep

type expr = _expr * Span.t * Data.fn
and _expr = 
  | Bop of expr * bop * expr
  | SectLeft of bop * expr
  | SectRight of expr * bop
  | Sect of bop
  | Uop of expr * uop
  | Dop of expr * dop * expr
  | Nop of nop

  | Lit of lit
  | Var of string
  | Let of stmt list * expr

and bop = 
  | Aop of aop
  | Cop of cop

(* arithmetic operators *)
and aop = 
  | Add
  | Sub
  | Mul

(* comparison operators *)
and cop = 
  | Eq
  | Neq
  | Gt
  | Lt
  | Ge
  | Le

(* unary operators *)
and uop = 
  | Dag
  | Mark
  | Plus
  | Star

(* dataflow operators *)
and dop = 
  | Tensor
  | Ponder
  | Fork
  | Pick
  | Cross
  | Guess
  
  | Jux
  | Contra
  | Union

(* nullary/stack operators *)
and nop = 
  | Gen | Fab | Exch | Elim | Cmp
  | Dup | Zap | Swap | Cons | Dip | Cat | Unit
  | DivMod | Lin | Parse | Show
  | Noop | Id | Ab

and lit = 
  | Int of int
  | String of string

  | Quote of expr
  | List of expr list

and stmt = _stmt * Metadata.Span.t
and _stmt = 
  | Def of string * expr
  [@@deriving show]

type toplevel = stmt list [@@deriving show]
