open! Batteries
open Uref

open Meta

type det = 
  | Monodet of {total : bool ; bare : bool}
  | Polydet of Var.t
  [@@deriving show]

type mode = {codet : det ; det : det} [@@deriving show]

type var = _var uref
and _var = 
  | Nom of rel list * string
  | Var of Var.t
and costack = var Ulist.t Ulist.t
and rel = costack * costack * mode
