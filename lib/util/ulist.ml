open! Batteries
open Uref

let pp_uref fmt x y = fmt x (uget y)

type ('a, 'b) t = ('a, 'b) _t uref
and ('a, 'b) _t = 
  | UCons of 'a * ('a, 'b) t
  | UNil
  | USeq of 'b [@@deriving show]

type ('a, 'b) ulist = ('a, 'b) t [@@deriving show]

let unil () = uref UNil
let ucons u us = uref @@ UCons (u, us)
let useq u = uref @@ USeq u

let ufront us = match uget us with
  | UCons (u, us) -> Either.Right (u, us)
  | USeq u -> Either.Left (Some u)
  | UNil -> Either.Left None

let rec umap ?(g=Fun.id) f us = uref @@ match uget us with
  | UCons (u, us) -> UCons (f u, umap ~g f us)
  | USeq u -> USeq (g u)
  | UNil -> UNil

let rec uiter ?(g=ignore) f us = match uget us with
  | UCons (u, us) -> f u; uiter ~g f us
  | USeq u -> g u
  | UNil -> ()

let rec unite unite_val r = 
  r |> Uref.unite ~sel:begin fun s t -> match s, t with
    | USeq _, USeq _ -> s
    | (UCons _ as x), USeq _ | USeq _, (UCons _ as x) -> x
    | UCons (u, us), UCons (v, vs) -> 
      unite_val u v;
      unite unite_val us vs;
      s
    | UCons _, UNil | UNil, UCons _ -> raise @@ Invalid_argument "unite"
    | USeq _, UNil | UNil, USeq _ | UNil, UNil -> UNil
  end
