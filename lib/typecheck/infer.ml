open! Batteries

open Syntax
open Ast

open Util
open Nuf
open System

let rec infer ctx (ast0, _sp, (i0, l0, o0)) p0 = match ast0 with
  | Bop ((_, _, (i1, l1, o1) as left), Aop _, (_, _, (i2, l2, o2) as right)) -> 
    let input, p1 = mk_unit_comb p0 in
    let output, p2 = comb_exact_1 Int p1 in
    p2 |> unify_comb i0 input
    >>= unify_comb i1 input
    >>= unify_comb i2 input
    >>= unify_comb o0 output
    >>= unify_comb o1 output
    >>= unify_comb o2 output
    >>= unify_comb l0 input
    >>= unify_comb l1 input
    >>= unify_comb l2 input
    >>= infer ctx left
    >>= infer ctx right
  
  
  | _ -> failwith "todo"
