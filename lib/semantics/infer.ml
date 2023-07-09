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
    let p = s >>: ufresh () in
    p =?= i1;
    s >>: p =?= o1
  
  | SectLeft (Aop _, (_, _, (i1, o1) as just)) -> 
    infer ctx uctx just;
    let u = mk_unital_costack () in
    u =?= i1;
    uref @@ System.Lit Int >: u =?= o1;
    let p = uref @@ System.Lit Int >: mk_poly_costack () in
    p =?= i0;
    p =?= o0
  
  | SectLeft (Cop _, (_, _, (i1, o1) as just)) -> 
    infer ctx uctx just;
    let u = mk_unital_costack () in
    u =?= i1;
    uref @@ System.Lit Int >: u =?= o1;
    let s = ufresh () in
    let p = s >>: ufresh () in
    uref @@ System.Lit Int >: p =?= i1;
    s >>: p =?= o1
  
  | SectRight ((_, _, (i1, o1) as just), Aop _) -> 
    infer ctx uctx just;
    let u = mk_unital_costack () in
    u =?= i1;
    uref @@ System.Lit Int >: u =?= o1;
    let p = uref @@ System.Lit Int >: mk_poly_costack () in
    p =?= i0;
    p =?= o0
  
  | SectRight ((_, _, (i1, o1) as just), Cop _) -> 
    infer ctx uctx just;
    let u = mk_unital_costack () in
    u =?= i1;
    uref @@ System.Lit Int >: u =?= o1;
    let s = ufresh () in
    let p = s >>: ufresh () in
    uref @@ System.Lit Int >: p =?= i1;
    s >>: p =?= o1
  
  | Sect Aop _ -> 
    let p = uref @@ System.Lit Int >: mk_poly_costack () in
    p =?= i0;
    uref @@ System.Lit Int >: p =?= o0
  
  | Sect Cop _ -> 
    let s = ufresh () in
    let p = s >>: ufresh () in
    uref @@ System.Lit Int >: (uref @@ System.Lit Int >: p) =?= i0;
    s >>: p =?= o0
  
  | _ -> failwith "todo"
