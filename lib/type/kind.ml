open! Batteries

open Util
open Meta

open Ulist
open Uref

exception DifferentlyKinded of string * string
exception CannotGeneralize of string * string

type 'a seq = ('a, unit) ulist [@@deriving show]
type var = _var uref
and _var = 
  | KStar
  | KVar of Var.t  (* note: seems KVar and KVarSpec are unnecessary *)
  | KVarSpec of Var.t
  | KArrow of t
  [@@deriving show]
and stack = var seq
and t = stack * stack

let rec unify ?(fvars=Var.checker ()) = 
  unite ~sel:begin fun n m -> 
    
    let err () = object
      val s1 = show__var n val s2 = show__var m
      method diffkind () = raise @@ DifferentlyKinded (s1, s2)
      method cannotgen () = raise @@ CannotGeneralize (s1, s2)
    end in

    match n, m with
    | KStar, KStar -> KStar
    | (KVar q as v), (KStar | KVar _ | KArrow _)
    | (KStar | KArrow _), (KVar q as v) -> 
      fvars#occurs q;
      v
    | (KVarSpec q as v), (KVar _)
    | (KVar _), (KVarSpec q as v) -> 
      fvars#occurs q;
      v
    | KVarSpec m1, KVarSpec m2 -> 
      if Var.equal m1 m2 then n
      else (err ())#cannotgen ()
    | KArrow (i1, o1), KArrow (i2, o2) -> 
      unify_stack ~fvars i1 i2;
      unify_stack ~fvars o1 o2;
      n
    | KVarSpec _, _ | _, KVarSpec _ -> (err ())#cannotgen ()
    | KArrow _, _ | _, KArrow _ -> (err ())#diffkind ()
end

and unify_stack ?(fvars=Var.checker ()) = 
  Ulist.unite (unify ~fvars)
