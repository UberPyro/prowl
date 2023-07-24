open! Batteries
open Printf
open Ucommon

module Make (U : UNIFIABLE) = struct

  type t = t_unif uref
  and t_unif = 
    | UCons of U.t * t
    | UNil
    | USeq of int
  type ulist = t
  type u = U.t

  type memo = (int, t) Hashtbl.t

  let memo_ : memo = Hashtbl.create 16
  let memo () = memo_
  let refresh_memo () = 
    U.refresh_memo ();
    Hashtbl.clear memo_

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

  let rec unify r = 
    Uref.unite ~sel:begin fun s t -> match s, t with
      | USeq _, USeq _ -> s
      | (UCons _ as x), USeq v | USeq v, (UCons _ as x) -> 
        occurs v (uref x);
        x
      | UCons (u, us), UCons (v, vs) -> 
        U.unify u v;
        unify us vs;
        s
      | UCons _, UNil | UNil, UCons _ -> 
        raise @@ UnifError "Cannot unify differently sized sequences"
      | USeq _, UNil | UNil, USeq _ | UNil, UNil -> UNil
    end r

  and occurs v = 
    let msg = 
      Printf.sprintf
        "Cannot unify a variable with a sequence that contains it" in
    uiter ~g:(assert_exn (UnifError msg) v) (U.occurs v)

  let rec extend unifier ulst vs = match uget ulst with
    | UCons (_, us) -> extend unifier us vs
    | UNil -> 
      UnifError "Cannot extend terminated difference list"
      |> raise
    | USeq _ -> unifier ulst vs

  let rec rebase base vs = match uget vs with
    | UCons (u, us) -> rebase base us |> ucons u
    | USeq _ | UNil -> base

  let rec generalize m t = match uget t with
    | USeq i -> 
        Hashtbl.find_option m i |> Option.default_delayed @@ fun () -> 
          let nu = ufresh () in
          Hashtbl.add m i nu;
          nu
    | UCons (u, us) -> uref @@ UCons (U.generalize (U.memo ()) u, generalize m us)
    | UNil -> unil ()
  
  let rec pretty out = uget %> function
    | UCons (u, us) -> 
      pretty out us;
      fprintf out " "; U.pretty out u
    | USeq j -> fprintf out "%d*" j
    | UNil -> fprintf out "."
  
  let rec atleast m = curry @@ Tuple2.mapn uget %> function
    | UCons (u, us), UCons (v, vs) -> U.atleast m u v && atleast m us vs
    | USeq i, USeq j -> Matcher.check m i j
    | USeq _, _ | UNil, UNil -> true
    | _, _ -> false

end

module type UNIF_LIST = sig
  include UNIFIABLE
  type u
  val unil : unit -> t
  val ucons : u -> t -> t
  val useq : int -> t
  val ufresh : unit -> t
  val usplit : ?acc:u list -> t -> u list * int option
  val ujoin : t -> u list -> t
  val rebase : t -> t -> t
  val map_hd : (u -> u) -> t -> t
  val upop : t -> u * t
end
