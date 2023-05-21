open! Batteries
open Uref

(* open Parse *)

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
  | Closure (* ast * env *) (* need a span to make ast *)
  | Thunk of fn * fn

and stack = value Depth_stack.t
and costack = stack Costack.t

and fn = costack -> costack LazyList.t * int * int
[@@deriving show]
