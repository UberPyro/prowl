(* Abelian Lists *)

open! Batteries
open Uref

exception AbelError

module type Abelian = sig
  type t [@@deriving show, eq]

  val add : t -> t -> t
  val inv : t -> t
  val empty : t
end

module Make(A : Abelian) = struct

  type uabel = _uabel uref
  and _uabel = 
    | Abel of A.t
    | AVar of int
  
  type aexpr = {
    vars : A.t Ptmap.t;
    const : A.t;
  }



end

(* trivial case *)
(* let sel_abel l r = match l, r with
  | _, AVar -> l
  | AVar, _ -> r
  | Abel i, Abel j when i = j -> l
  | _ -> raise AbelError *)
