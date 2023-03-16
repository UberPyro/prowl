open! Batteries
open Ulist
open Uref

open Meta

exception DifferentlyKinded of string * int * string * int
exception Clash of string * string

type det = 
  | Monodet of {total : bool ; bare : bool}
  | Polydet of Var.t
  [@@deriving show]

type mode = {codet : det uref ; det : det uref} [@@deriving show]

type var = _var uref [@@deriving show]
and _var = 
  | Nom of rel list * string
  | Var of Var.t
  [@@deriving show]
and costack = var Ulist.t Ulist.t
and rel = costack * costack * mode [@@deriving show]

let unify_det r = 
  r |> unite ~sel:begin fun d1 d2 -> match d1, d2 with
    | Polydet _, Polydet _ -> d1
    | (Monodet _ as m), Polydet _ | Polydet _, (Monodet _ as m) -> m
    | Monodet c0, Monodet c1 -> Monodet {
      total = c0.total && c1.total; 
      bare = c0.bare && c1.bare;
    }
  end

let unify_mode m0 m1 = 
  unify_det m0.codet m1.codet;
  unify_det m0.det m1.det

let iter12 f (x, y, _) = ignore (f x, f y)

let rec unify r = 
  r |> unite ~sel:begin fun u1 u2 -> match u1, u2 with
    | Nom (f0, n0), Nom (f1, n1) -> 
      if List.(length f0 <> length f1)
      then raise @@ DifferentlyKinded (n0, List.length f0, n1, List.length f1)
      else if n0 <> n1
      then raise @@ Clash (n0, n1)
      else List.iter2 begin fun (i0, o0, m0) (i1, o1, m1) -> 
        unify_costack i0 i1;
        unify_costack o0 o1;
        unify_mode m0 m1
      end f0 f1;
      u1
    | (Var v as u), (Nom _ as n) | (Nom _ as n), (Var v as u) -> 
      occurs v (uref n);
      u
    | Var _, Var _ -> u1
  end

and unify_costack r = 
  Ulist.unite (Ulist.unite unify occurs) (Ulist.occurs occurs) r

and occurs (v : Var.t) : var -> unit = uget %> function
  | Var w -> assert_exn (Var.OccursError (Var.show v)) v w
  | Nom (f, _) -> List.iter (iter12 (occurs_costack v)) f

and occurs_costack v = Ulist.occurs (Ulist.occurs occurs) v
