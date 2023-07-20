open! Batteries
open Ucommon

type 'a t = 'a _t uref
and 'a _t = 
  | UCons of 'a * 'a t
  | UNil
  | USeq of int [@@deriving show]

type 'a ulist = 'a t [@@deriving show]

let unil () = uref UNil
let ucons u us = uref @@ UCons (u, us)
let useq u = uref @@ USeq u
let ufresh () = useq @@ unique ()

let rec uiter ?(g=ignore) f us = match uget us with
  | UCons (u, us) -> f u; uiter ~g f us
  | USeq u -> g u
  | UNil -> ()

let rec copy copy_elem us = match uget us with
  | UCons (u, us) -> ucons (copy_elem u) (copy copy_elem us)
  | USeq k -> useq k
  | UNil -> us

let usome u = ucons u @@ ufresh ()
let ujust u = ucons u @@ unil ()

let map_hd f us = match uget us with
  | UCons (x, xs) -> uref @@ UCons (f x, xs)
  | UNil | USeq _ -> us

let dup_hd us = match uget us with
  | UCons (x, _) -> uref @@ UCons (x, us)
  | UNil | USeq _ -> raise @@ Invalid_argument "dup_hd"

let upop us = match uget us with
  | UCons (x, xs) -> x, xs
  | UNil | USeq _ -> raise @@ Invalid_argument "upop"

let rec usplit ?(acc=[]) us = match uget us with
  | UCons (x, xs) -> usplit ~acc:(x :: acc) xs
  | UNil -> acc, None
  | USeq k -> acc, Some k

let rec ujoin acc = function
  | x :: xs -> ujoin (ucons x acc) xs
  | [] -> acc

let assert_exn exn x y = if x = y then raise exn

let rec unify unify_elem occurs_elem = 
  Uref.unite ~sel:begin fun s t -> match s, t with
    | USeq _, USeq _ -> s
    | (UCons _ as x), USeq v | USeq v, (UCons _ as x) -> 
      occurs occurs_elem v (uref x);
      x
    | UCons (u, us), UCons (v, vs) -> 
      unify_elem u v;
      unify unify_elem occurs_elem us vs;
      s
    | UCons _, UNil | UNil, UCons _ -> 
      raise @@ UnifError "Cannot unify differently sized sequences"
    | USeq _, UNil | UNil, USeq _ | UNil, UNil -> UNil
  end

and occurs occurs_elem v = 
  let msg = 
    Printf.sprintf
      "Cannot unify a variable with a sequence that contains it" in
  uiter ~g:(assert_exn (UnifError msg) v) (occurs_elem v)

let rec extend unifier ulst vs = match uget ulst with
  | UCons (_, us) -> extend unifier us vs
  | UNil -> 
    UnifError "Cannot extend terminated difference list"
    |> raise
  | USeq _ -> unifier ulst vs

let rec rebase base vs = match uget vs with
  | UCons (u, us) -> rebase base us |> ucons u
  | USeq _ | UNil -> base
