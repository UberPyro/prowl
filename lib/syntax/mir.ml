open! Batteries

open Metadata

type expr = _expr * Span.t
and _expr = [
  | Prim.lit
  | Prim.op
  | expr Prim.dag
  | expr Ast.core
]
