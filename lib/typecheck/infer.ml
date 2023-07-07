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
    let poly_stack_deref = search_stack poly_stack p4 in
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
  
  | SectLeft (Aop _, (_, _, (i1, _, o1) as right)) -> 
    let poly, p1 = mk_poly_poly p0 in
    let base, p2 = comb_push poly Int p1 in
    let input, p3 = mk_unit_comb p2 in
    let output, p4 = comb_exact_1 Int p3 in
    unify_comb i0 base p4
    >>= unify_comb o0 base
    >>= unify_comb i1 input
    >>= unify_comb o1 output
    >>= unify_comb l0 poly
    >>= infer ctx right
  
  | SectLeft (Cop _, (_, _, (i1, _, o1) as right)) -> 
    let poly, p1 = mk_poly_comb p0 in
    let poly_stack, p2 = mk_poly_stack p1 in
    let poly_stack_deref = search_stack poly_stack p2 in
    let poly_stacked, p3 = comb_costack_push poly poly_stack_deref p2 in
    let poly_2stacked, p4 = comb_costack_push poly_stacked poly_stack_deref p3 in
    let input, p5 = mk_unit_comb p4 in
    let output, p6 = comb_exact_1 Int p5 in
    let poly_stacked_int, p7 = comb_push poly_stacked Int p6 in
    unify_comb i0 poly_stacked_int p7
    >>= unify_comb o0 poly_2stacked
    >>= unify_comb i1 input
    >>= unify_comb o1 output
    >>= unify_comb l0 poly_stacked
    >>= infer ctx right
  
  | SectRight ((_, _, (i1, _, o1) as left), Aop _) -> 
    let poly, p1 = mk_poly_poly p0 in
    let base, p2 = comb_push poly Int p1 in
    let input, p3 = mk_unit_comb p2 in
    let output, p4 = comb_exact_1 Int p3 in
    unify_comb i0 base p4
    >>= unify_comb o0 base
    >>= unify_comb i1 input
    >>= unify_comb o1 output
    >>= unify_comb l0 poly
    >>= infer ctx left
  
  | SectRight ((_, _, (i1, _, o1) as left), Cop _) -> 
    let poly, p1 = mk_poly_comb p0 in
    let poly_stack, p2 = mk_poly_stack p1 in
    let poly_stack_deref = search_stack poly_stack p2 in
    let poly_stacked, p3 = comb_costack_push poly poly_stack_deref p2 in
    let poly_2stacked, p4 = comb_costack_push poly_stacked poly_stack_deref p3 in
    let input, p5 = mk_unit_comb p4 in
    let output, p6 = comb_exact_1 Int p5 in
    let poly_stacked_int, p7 = comb_push poly_stacked Int p6 in
    unify_comb i0 poly_stacked_int p7
    >>= unify_comb o0 poly_2stacked
    >>= unify_comb i1 input
    >>= unify_comb o1 output
    >>= unify_comb l0 poly_stacked
    >>= infer ctx left
  
  | Sect Aop _ -> 
    let poly, p1 = mk_poly_poly p0 in
    let poly1, p2 = comb_push poly Int p1 in
    let poly2, p3 = comb_push poly1 Int p2 in
    unify_comb i0 poly2 p3
    >>= unify_comb o0 poly1
    >>= unify_comb l0 poly
  
  | Sect Cop _ -> 
    let stunted, p1 = mk_poly_comb p0 in
    let poly_stack, p2 = mk_poly_stack p1 in
    let poly_stack_deref = search_stack poly_stack p2 in
    let stack, p3 = comb_costack_push stunted poly_stack_deref p2 in
    let stack_double, p4 = comb_costack_push stack poly_stack_deref p3 in
    let stack_int, p5 = comb_push stack Int p4 in
    let stack_int_int, p6 = comb_push stack_int Int p5 in
    unify_comb i0 stack_int_int p6
    >>= unify_comb o0 stack_double
    >>= unify_comb l0 stack
  
  | Uop ((_, _, (i1, l1, o1) as just), Dag) -> 
    unify_comb i0 o1 p0
    >>= unify_comb i1 o0
    >>= unify_comb l0 l1
    >>= infer ctx just
  
  | Uop ((_, _, (i1, l1, o1) as just), (Mark | Star | Plus)) -> 
    unify_comb i1 o1 p0
    >>= unify_comb i0 i1
    >>= unify_comb o0 o1
    >>= unify_comb l0 l1
    >>= infer ctx just
  
  (* | Dop ((_, _, (i1, _, o1) as left), Jux, (_, _, (i2, _, o2) as right)) ->  *)
  
  | _ -> failwith "todo"

and infer_jux (i1, l1, o1) (i2, l2, o2) (i0, l0, o0) p0 = 
  let* p1 = unify_comb o1 i2 p0 >>= unify_comb i1 i0 >>= unify_comb o2 o0 in
  begin
    let c, p2 = mk_poly_stunted p1 in
    let stunted_comb = search_comb c p2 in
    let depth_comb = search_comb l1 p2 in
    let excess_comb, p3 = comb_register (depth_comb @ stunted_comb) p2 in
    unify_comb l1 l0 p3 >>= unify_comb l2 excess_comb
  end <|> begin
    let c, p2 = mk_poly_poly p1 in
    let comb = search_comb c p2 in
    let depth_comb = search_comb l2 p2 in
    let excess_comb, p3 = comb_register (depth_comb @ comb) p2 in
    unify_comb l2 l0 p3 >>= unify_comb l1 excess_comb
  end
