open! Batteries

open Syntax
open Ast
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



(* let exec ctx nuf (e0, _, (_, _)) i0 = match e0 with
  | Bop (e1, op, e2) -> 

  | _ -> failwith "todo" *)
