open! Batteries

open Syntax
open Util

type fn = costack -> costack Enum.t [@@deriving show]

and costack = int * stack
and stack = int list

type value = 
  | VInt
  | VString
  | VQuote of callable
  | VList of callable
  | VVar

and callable = 
  | Closure of Ast.expr * context
  | Program of fn * fn

and context = (string, fn) Ouro.t * value Nuf.t
