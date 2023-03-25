open! Batteries

open Meta

type literal = [
  | `int of int
  | `float of float
  | `char of char
  | `string of string
] [@@deriving show]

type expr = _expr * Span.t [@@deriving show]
and _expr = [
  | literal
  | `var of string
  | `id of string

  | `jux of expr list
  | `dag of expr

  | `quote of expr
  | `list of expr list

  | `bind_var of (string * expr) list * expr
  | `bind_uvar of string list * expr

  | `unop of expr * string
  | `binop of expr * string * expr
  | `sectLeft of string * expr
  | `sectRight of expr * string
  | `sect of string
  | `arrow of expr * expr
] [@@deriving show]
