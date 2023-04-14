open! Batteries
open Uref

open Syntax

type _val = 
  | VInt of int
  | VStr of string
  | VQuote of callable
  | VList of callable

and callable = 
  | Closure of Ast.expr * Context.t
  | Thunk of fn * fn

and value = _val uref

and stack = value list
and costack = int * stack

and fn = costack -> costack Enum.t
