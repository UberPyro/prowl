open! Batteries
open Uref

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
  | Closure
  | Thunk of fn * fn

and stack = value Stack.t
and costack = stack Costack.t

and fn = costack -> costack LazyList.t * int * int
