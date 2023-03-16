open! Batteries

open Meta

type monodet = {total : bool ; bare : bool}

type unification_variable = [
  | `var of Var.t
  | `stack of Var.t
  | `costack of Var.t
] [@@deriving show]

type lexical_variable = [
  | `id of string
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

type expr = _expr * Span.t [@@deriving show]
and _expr = [
  | variable
  | literal

  | `jux of expr list
  | `dag of expr
  | `prime of expr

  | `quote of expr
  | `list of expr list
  | `block of expr * monodet * monodet

  | `bind_var of (string * expr) list * expr
  | `bind_uvar of Var.t list * expr

  | `unop of expr * string
  | `binop of expr * string * expr
  | `sectLeft of string * expr
  | `sectRight of expr * string
  | `sect of string
  | `arrow of expr * expr
] [@@deriving show]
