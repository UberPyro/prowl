(* Abelian Lists *)

open! Batteries
open Uref

exception AbelError

type uabel = _uabel uref
and _uabel = 
  | Abel of int
  | AVar of int

type 'a aexpr = {
  vars : 'a;  (* int array | int Ptmap.t *)
  const : int;
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
      | Some v' -> Some (v' - v)
      | None -> Some (-v)
    end a
  end r.vars l.vars;
  const = l.const - r.const
}
