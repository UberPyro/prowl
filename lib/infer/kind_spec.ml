open! Batteries

open Util.Ulist
open Syntax
open Meta
open Type
open Iter

open Uref

let rec derive ((k_, sp) : Ast.kind) : Kind.t = match k_ with
  | `var -> Kind.lit Kind.KStar
  | `seq -> Kind.start ()
  | `jux [] -> Kind.connected ()
  (* | `jux (h :: t) -> iter_pairs begin fun (a, _) (b, _) -> 
    Kind.connect a b
  end (fun (x, _) (y, _) -> fst x, snd y) h t *)
