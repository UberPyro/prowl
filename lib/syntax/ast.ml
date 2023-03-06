open! Batteries

open Meta
open Type

type det = 
  | Monodet of {total : bool ; bare : bool}
  | Polydet of Var.t
  [@@deriving show]

type mode = {codet : det ; det : det} [@@deriving show]

type kind = _kind * Span.t [@@deriving show]
and _kind = [
  | `var
  | `seq
  | `jux of kind list
  | `dag
] [@@deriving show]

and ty = _ty * mode * Span.t * Kind.t [@@deriving show]
and _ty = [
  | `id of string
  | `var of Var.t
  | `stack of Var.t
  | `costack of Var.t
  | `jux of ty list
  | `dag of ty
  | `quote of ty
  | `list of ty
] [@@deriving show]

and data = _data * Span.t [@@deriving show]
and _data = Tag of ty * string [@@deriving show]

type unification_variable = [
  | `var of Var.t
  | `stack of Var.t
  | `costack of Var.t
] [@@deriving show]

type lexical_variable = [
  | `id of string
  | `tag of string
] [@@deriving show]

type variable = [
  | unification_variable 
  | lexical_variable
] [@@deriving show]

type literal = [
  | `int of int
  | `float of float
  | `char of char
  | `string of string
] [@@deriving show]

type expr = _expr * Span.t * unit [@@deriving show]
and _expr = [
  | variable
  | literal

  | `jux of expr list
  | `dag of expr
  | `prime of expr

  | `quote of expr
  | `list of expr list
  | `block of expr * mode

  | `bind_var of (string * ty option * expr) list * expr
  | `bind_uvar of Var.t list * expr

  | `unop of expr * string
  | `binop of expr * string * expr
  | `sectLeft of string * expr
  | `sectRight of expr * string
  | `sect of string
  | `arrow of expr * expr
] [@@deriving show]

type def = _def * Span.t [@@deriving show]
and _def = [
  | `valdef of string * ty option * expr
  | `typedef of ty
  | `data of data
] [@@deriving show]
