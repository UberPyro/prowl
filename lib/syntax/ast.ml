open! Batteries

open Meta

type expr = _expr * Span.t [@@deriving show]
and _expr = [
  | `int
  | `string
  | `id of string

  | `jux of expr list
  | `dag of expr

  | `quote of expr
  | `list of expr list

  | `bind_var of (string * expr) list * expr
  | `bind_uvar of string list * expr

  | `binop of expr * string * expr
  | `sectLeft of string * expr
  | `sectRight of expr * string
  | `sect of string
  | `arrow of expr * expr
] [@@deriving show]
