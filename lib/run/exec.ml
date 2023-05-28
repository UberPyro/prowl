open! Batteries
open Uref

open Parse
open Metadata

module Code = Ast.Make(Span)

let pp_uref fmt x y = fmt x (uget y)

type value = _value uref
and _value = 
  | Lit of lit
  | Exec
  | Free
  | Empty

and lit = 
  | Int of int
  | String of string
  | List of exec

and exec = 
  | Closure of Code.expr * context
  | Thunk of mode * mode

and stack = |   (* TODO: Implement diff. lists & stacks *)
and costack = | (* TODO: Implement diff. costacks *)

and mode = costack LazySet.t uref -> costack LazySet.t uref -> unit
and context = (Code.expr, _value) Context.t
[@@deriving show]
