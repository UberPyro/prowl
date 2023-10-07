[@@@warning "-32"]
open Metadata
open Types

type det = 
  | DLit of Det.t
  | DAnd of det * det
  | DXor of det * det
  | DVar of string

type ty_expr = 
  | Explicit of costack_expr * costack_expr * det * det
  | ImplicitCostack of stack_expr list * stack_expr list * det * det
  | ImplicitStack of value_expr list * value_expr list * det * det
and costack_expr = string option * stack_expr list
and stack_expr = string option * value_expr list
and value_expr = 
  | TyInt
  | TyString
  | TyQuote of ty_expr
  | TyList of ty_expr
  | TyVal of string

type expr = _expr * Span.t * Fn.t
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
  | Ex of string * expr * bool
  | Each of expr * string * bool

and bop = 
  | Aop of aop
  | Cop of cop
  | Lop of lop

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

(* lifted operators *)
and lop = 
  | Cat
  | Alt

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
  | Union

(* nullary/stack operators *)
and nop = 
  | Gen | Fab | Exch | Elim | Cmp
  | Dup | Zap | Swap | Cons | Dip | Unit | Call
  | DivMod | Parse | Show | Push | Enq
  | Noop | Id | Ab | San

and lit = 
  | Int of int
  | String of string

  | Quote of expr

and stmt = _stmt * Metadata.Span.t
and _stmt = 
  | Def of string * ty_expr option * expr

type toplevel = stmt list
