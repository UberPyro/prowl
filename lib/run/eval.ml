open! Batteries
open Uref
open Either
open Enum

open Syntax

type _val = 
  | VInt of int
  | VStr of string
  | VQuote of callable
  | VList of callable

and callable = 
  | Closure of Ast.expr * Context.t
  | Thunk of fn * fn * arity

and value = _val uref

and stack = value list
and costack = int * stack

and fn = costack -> costack Enum.t
and arity = int list * int list

(* let rec exec (ctx : Context.t) (e0 : Ast.expr) : arity * fn = 
  match fst e0 with
  | Bop (e1, Ponder, e2) ->  *)
    (* let a1, f1 = exec ctx e1 *)
