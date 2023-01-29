open! Batteries
open Type

type value_type = _value_type * Span.t
and _value_type = 
  | TId of string
  | TVar of string
  | TQuote of relation
  | TList of relation

and stack_head = value_type list
and costack_head = (seq_tail * stack_head) list
and seq_tail = 
  | Has of int
  | Bottom

and relation = _relation * Span.t
and _relation = 
  | ImplStack of stack_head twin
  | ImplCostack of costack_head twin
  | Expl of (seq_tail * costack_head) twin

type expr = _expr * Span.t * (costack * costack)
and _expr = 
  | Id of string
  | Var of string
  | Seq of int
  | Coseq of int
  | Quote of expr
  | List of expr list

  | Int of int
  | Float of float
  | Char of char
  | String of string

  | Tag of string

  | Cat of expr list
  | Binop of expr * string * expr
  | Unop of string * expr
  | LeftSect of string * expr
  | RightSect of expr * string
  | Sect of string
  | Meet of expr list
  | Join of expr list
  | Conj of string * expr

  | Let of (string * expr) list * expr
  | Lam of pat * expr
  | Pick of expr list
  | Ponder of expr list

and pat = _pat * Span.t
and _pat = 
  | PId of string
  | PInt of int
  | PFloat of float
  | PChar of char
  | PString of string
  | PQuote of string
  (* | PList of string list *)
  | PCat of pat list
  (* | PMeet of pat list *)
  (* | PJoin of pat list *)
  | PConj of pat * pat
  (* | PDisj of pat * pat
  | PSnoc of pat * pat *)

type parameter = 
  | PVar of string
  | PQuote of int * int

type component = _component * Span.t
and _component = 
  | Def of string * expr
  (* | Spec of string * relation *)
  | Alias of string * parameter list * value_type list list
  | Tagspec of string * parameter list * value_type list list * string
