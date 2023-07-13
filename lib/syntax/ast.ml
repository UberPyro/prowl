open Metadata
[@@@warning "-32"]

type ty_expr = 
  | Explicit of costack_expr * costack_expr
  | ImplicitCostack of stack_expr list * stack_expr list
  | ImplicitStack of value_expr list * value_expr list
and costack_expr = string option * stack_expr list
and stack_expr = string option * value_expr list
and value_expr = 
  | TyInt
  | TyString
  | TyQuote of ty_expr
  | TyList of ty_expr
  | TyVal of string
  [@@deriving show]

type expr = _expr * Span.t * Types.fn
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
  | Ex of string * expr
  | TypedEx of (string * ty_expr) list * expr
  | UVar of string

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
  | Apply
  | Induce

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
  | Noop | Id | Ab | Ap

and lit = 
  | Int of int
  | String of string

  | Quote of expr
  | List of expr list

and stmt = _stmt * Metadata.Span.t
and _stmt = 
  | Def of string * ty_expr option * expr
  [@@deriving show]

type toplevel = stmt list [@@deriving show]
