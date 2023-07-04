open! Batteries

open Syntax
open Ast

open Util
open Nuf
open System

let rec infer ctx (ast0, _sp, (i0, l0, o0)) p0 = match ast0 with
  | Bop ((_, _, (i1, _, o1) as left), Aop _, (_, _, (i2, _, o2) as right)) -> 
    let input, p1 = mk_unit_comb p0 in
    let output, p2 = comb_exact_1 Int p1 in
    let poly, p3 = mk_poly_poly p2 in
    let out0, p4 = comb_push poly Int p3 in
    unify_comb i0 poly p4
    >>= unify_comb i1 input
    >>= unify_comb i2 input
    >>= unify_comb o0 out0
    >>= unify_comb o1 output
    >>= unify_comb o2 output
    >>= unify_comb l0 poly
    >>= infer ctx left
    >>= infer ctx right
  
  | Bop ((_, _, (i1, _, o1) as left), Cop _, (_, _, (i2, _, o2) as right)) -> 
    let input, p1 = mk_unit_comb p0 in
    let output, p2 = comb_exact_1 Int p1 in
    let poly, p3 = mk_poly_comb p2 in
    let poly_stack, p4 = mk_poly_stack p3 in
    let[@warning "-8"] _, (S poly_stack_deref) = search poly_stack p4 in
    let poly_stacked, p5 = comb_costack_push poly poly_stack_deref p4 in
    let poly_2stacked, p6 = comb_costack_push poly_stacked poly_stack_deref p5 in
    unify_comb i0 poly_stacked p6
    >>= unify_comb o0 poly_2stacked
    >>= unify_comb i1 input
    >>= unify_comb i2 input
    >>= unify_comb o1 output
    >>= unify_comb o2 output
    >>= unify_comb l0 poly_stacked
    >>= infer ctx left
    >>= infer ctx right
  
  | _ -> failwith "todo"
