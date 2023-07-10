open! Batteries
open Printf
open Uref

open Util
open Ull

type value = value_ uref
and value_ = 
  | Lit of lit
  | Con of fn * con
  | Var of int
and lit = Int | String
and con = Quote | List
and stack = value ulist
and costack = stack ulist
and fn = costack * costack
  [@@deriving show]

let rec unify v0 = v0 |> Uref.unite ~sel:begin curry @@ function
  | Var _ as v, Var _ -> v
  | Var k, v | v, Var k -> 
    occurs_val k (uref v);
    v
  | Lit l1 as v, Lit l2 when l1 = l2 -> v
  | Con (f1, c1) as v, Con (f2, c2) when c1 = c2 -> 
    unify_fn f1 f2; 
    v
  | v1, v2 -> 
    failwith @@ sprintf 
      "Cannot unify types [%s] and [%s]"
      (show_value_ v1)
      (show_value_ v2)
end

and occurs_val k1 = uget %> function
  | Var k2 when k1 = k2 -> failwith "Occurs error"
  | Con (f, _) -> occurs_fn k1 f
  | _ -> ()

and (-?-) s0 = Ull.unite unify occurs_val s0
and occurs_stack k = Ull.occurs occurs_val k
and (=?=) c0 = Ull.unite (-?-) occurs_stack c0
and occurs_costack k = Ull.occurs occurs_stack k

and unify_fn (c1, c2) (d1, d2) = 
  c1 =?= d1;
  c2 =?= d2;

and occurs_fn k (c1, c2) = 
  occurs_costack k c1;
  occurs_costack k c2

let mk_unital_costack () = ucons (unil ()) (ufresh ())
let mk_poly_costack () = ucons (ufresh ()) (ufresh ())
let (@>) v_ = map_hd (ucons (uref @@ v_))
let mk_var () = uref @@ Var (unique ())
let (@@>) v = map_hd (ucons v)
let (@>>) s = ucons s
