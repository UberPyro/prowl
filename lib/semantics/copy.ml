open! Batteries
open Uref

open System

open Unify
open Ulist

let rec fn f = Tuple2.mapn costack f

and costack c = copy stack c
and stack s = copy value s

and value v = match uget v with
  | Con (f, c) -> uref @@ Con (fn f, c)
  | Lit _ -> v
  | Var k -> uref @@ Var k
