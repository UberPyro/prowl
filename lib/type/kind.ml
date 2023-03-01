open! Batteries

open Ulist
open Uref

type 'a seq = ('a, unit) ulist [@@deriving show]
type var = _var uref
and _var = 
  | KStar
  | KVar of Meta.t
  | KArrow of arrow
  [@@deriving show]
and stack = var seq
and arrow = stack * stack

let rec unify r = 
  r |> unite ~sel:begin fun n m -> match n, m with
    | KStar, KStar -> KStar
    | KVar _ as v, _ | _, (KVar _ as v) -> v
    | KArrow (i1, o1), KArrow (i2, o2) -> 
      unify_stack i1 i2;
      unify_stack o1 o2;
      n
    | _ -> 
      Printf.sprintf 
        "Cannot unify kinds [%s] and [%s]"
        (show__var n) (show__var m)
      |> failwith
end

and unify_stack r = Ulist.unite unify r
