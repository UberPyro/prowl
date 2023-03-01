open! Batteries

open Ulist
open Uref

type 'a seq = ('a, unit) ulist [@@deriving show]
type var = _var uref
and _var = 
  | KStar
  | KVar of Meta.t
  [@@deriving show]
type stack = var seq


