open! Batteries

open Metadata

type expr = _expr * Span.t [@@deriving show]
and _expr = [
  | Prim.word
  | Prim.op
  | expr Ast.core
] [@@deriving show]
