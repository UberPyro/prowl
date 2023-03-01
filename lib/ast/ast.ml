open! Batteries

type kind = _kind * Span.t [@@deriving show]
and _kind = [
  | `var
  | `seq
  | `jux of kind list
  | `dag of kind
] [@@deriving show]

and ty = _ty * Mode.t * Span.t * unit [@@deriving show]
and _ty = [
  | `id of string
  | `var of Meta.t
  | `stack of Meta.t
  | `costack of Meta.t
  | `jux of ty list
  | `dag of ty
  | `quote of ty
  | `list of ty
] [@@deriving show]

and data = _data * Span.t [@@deriving show]
and _data = Tag of ty * string [@@deriving show]

type variable = [
  | `id of string
  | `var of Meta.t
  | `stack of Meta.t
  | `costack of Meta.t
  | `tag of string
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
  | `block of expr * Mode.t

  | `binding of (string * ty option * expr) list * expr

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
