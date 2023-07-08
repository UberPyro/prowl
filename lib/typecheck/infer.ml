open! Batteries

open Syntax
open Ast

open Util
open Nuf
open System

let rec infer ctx (ast, _, (i0, o0)) p0 = match ast with
  | Bop ((_, _, (i1, o1) as left), Aop _, (_, _, (i2, o2) as right)) -> 
    let unit, p1 = reg_unit_as_comb p0 in
    let int, p2 = reg_comb [Elem [Elem Int]] p1 in
    infer ctx left p2 >>= infer ctx right >>= unify_chain [
      i0, unit;
      i1, unit;
      i2, unit;
      o0, int;
      o1, int;
      o2, int;
    ]
  
  | Bop ((_, _, (i1, o1) as left), Cop _, (_, _, (i2, o2) as right)) -> 
    let poly_stack, p1 = reg_poly_stack p0 in
    let comb = [Elem [SeqVar poly_stack]] in
    let c1, p2 = reg_comb comb p1 in
    let c2, p3 = reg_comb (comb @ comb) p2 in
    let unit, p4 = reg_unit_as_comb p3 in
    let int, p5 = reg_unit_as_comb p4 in
    infer ctx left p5 >>= infer ctx right >>= unify_chain [
      i0, c1;
      i1, unit;
      i2, unit;
      o0, c2;
      o1, int;
      o2, int;
    ]

  | _ -> failwith "todo"
