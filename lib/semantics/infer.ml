open! Batteries
open Uref

open Syntax
open System
module Tast = Ast.Make(struct type t = fn[@@deriving show] end)
open Tast

open Util
open Ull

let rec infer ctx uctx (ast, _sp, (i0, o0)) = match ast with
  | Bop ((_, _, (i1, o1) as left), Aop _, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let u = mk_unital_costack () in
    u =?= i1;
    u =?= i2;
    let z = uref @@ System.Lit Int >: u in
    z =?= o1;
    z =?= o2;
    let p = mk_poly_costack () in
    p =?= i0;
    uref @@ System.Lit Int >: p =?= o0
  
  | Bop ((_, _, (i1, o1) as left), Cop _, (_, _, (i2, o2) as right)) -> 
    infer ctx uctx left;
    infer ctx uctx right;
    let u = mk_unital_costack () in
    u =?= i1;
    u =?= i2;
    let z = uref @@ System.Lit Int >: u in
    z =?= o1;
    z =?= o2;
    let s = ufresh () in
    let p1 = s >>: ufresh () in
    p1 =?= i1;
    s >>: p1 =?= o1
  
  | _ -> failwith "todo"
