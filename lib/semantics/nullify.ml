open! Batteries
open Uref

open System

open Util
open Ull

let rec nullify_fn fn = Tuple2.mapn nullify_costack fn

and nullify_costack c0 = match uget c0 with
  | UCons (u, us) -> ucons (nullify_stack u) (nullify_costack us)
  | USeq _ | UNil -> unil ()

and nullify_stack s0 = match uget s0 with
  | UCons (u, us) -> ucons (nullify_value u) (nullify_stack us)
  | USeq _ | UNil -> unil ()

and nullify_value v0 = match uget v0 with
  | Var _ -> uref @@ Lit Void
  | _ -> v0
