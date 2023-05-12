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
  
  type 'a aexpr = {
    vars : 'a;  (* A.t array | A.t Ptmap.t *)
    const : A.t;
  }

  let count = 
    Array.fold_left begin fun a x -> 
      Ptmap.update x begin function
        | Some c -> Some (c + 1)
        | None -> Some 0
      end a
    end Ptmap.empty

  let one_side l r = {
    vars = Ptmap.fold begin fun k v a ->
      Ptmap.update k begin function
        | Some v' -> Some (A.add v' (A.inv v))
        | None -> Some (A.inv v)
      end a
    end r.vars l.vars;
    const = A.add l.const (A.inv r.const)
  }

end
