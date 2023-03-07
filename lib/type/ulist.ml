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

(* let ufront us = match uget us with
  | UCons (u, us) -> Either.Right (u, us)
  | USeq u -> Either.Left (Some u)
  | UNil -> Either.Left None

let rec umap ?(g=Fun.id) f us = uref @@ match uget us with
  | UCons (u, us) -> UCons (f u, umap ~g f us)
  | USeq u -> USeq (g u)
  | UNil -> UNil *)

let rec uiter ?(g=ignore) f us = match uget us with
  | UCons (u, us) -> f u; uiter ~g f us
  | USeq u -> g u
  | UNil -> ()

let rec unite unite_val occurs_val = 
  Uref.unite ~sel:begin fun s t -> match s, t with
    | USeq _, USeq _ -> s
    | (UCons _ as x), USeq v | USeq v, (UCons _ as x) -> 
      uiter ~g:(fun w -> if v = w then raise @@ Var.OccursError (Var.show v))
        (occurs_val v) (uref x);
      x
    | UCons (u, us), UCons (v, vs) -> 
      unite_val u v;
      unite unite_val occurs_val us vs;
      s
    | UCons _, UNil | UNil, UCons _ -> raise @@ Invalid_argument "unite"
    | USeq _, UNil | UNil, USeq _ | UNil, UNil -> UNil
  end
