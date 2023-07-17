open! Batteries
open Uref

open System

open Unify
open Ulist
open Umode

let rec fn (c1, c2, x) = costack c1, costack c2, copy_ubool x

and costack c = copy stack c
and stack s = copy value s

and value v = match uget v with
  | Con (f, c) -> uref @@ Con (fn f, c)
  | Lit _ -> v
  | Var k -> uref @@ Var k
