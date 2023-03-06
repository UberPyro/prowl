open! Batteries

open Util
open Meta

open Ulist
open Uref

exception DifferentlyKinded of string * string

type 'a seq = ('a, Var.t) ulist [@@deriving show]
type var = _var uref
and _var = 
  | KStar
  | KArrow of t
  [@@deriving show]
and stack = var seq
and t = stack * stack

let rec unify r = 
  r |> unite ~sel:begin fun n m -> match n, m with
    | KStar, KStar -> KStar
    | KArrow (i1, o1), KArrow (i2, o2) -> 
      unify_stack i1 i2;
      unify_stack o1 o2;
      n
    | KArrow _, _ | _, KArrow _ -> 
      raise @@ DifferentlyKinded (show__var n, show__var m)
end

and unify_stack s = Ulist.unite unify s

let create () = useq (Var.fresh ())
let free () = create (), create ()
let connected () =
  let s = create () in
  s, s
let connect (_, o) (i, _) = unify_stack o i
let lit x = Tuple2.map2 (ucons (uref x)) (free ())
let start () = unil (), create ()
let swap = Tuple2.swap
