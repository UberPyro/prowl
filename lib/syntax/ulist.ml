open! Batteries
open Uref

open Meta

let pp_uref fmt x y = fmt x (uget y)

type 'a t = 'a _t uref
and 'a _t = 
  | UCons of 'a * 'a t
  | UNil
  | USeq of Var.t [@@deriving show]

type 'a ulist = 'a t [@@deriving show]

let unil () = uref UNil
let ucons u us = uref @@ UCons (u, us)
let useq u = uref @@ USeq u
let ufresh () = useq (Var.fresh ())

(* let rec uiter ?(g=ignore) f us = match uget us with
  | UCons (u, us) -> f u; uiter ~g f us
  | USeq u -> g u
  | UNil -> () *)

let rec unite unite_val = 
  Uref.unite ~sel:begin fun s t -> match s, t with
    | USeq _, USeq _ -> s
    | (UCons _ as x), USeq _ | USeq _, (UCons _ as x) -> x
    | UCons (u, us), UCons (v, vs) -> 
      unite_val u v;
      unite unite_val us vs;
      s
    | UCons _, UNil | UNil, UCons _ -> raise @@ Invalid_argument "unite"
    | USeq _, UNil | UNil, USeq _ | UNil, UNil -> UNil
  end
