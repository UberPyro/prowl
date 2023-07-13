open! Batteries
open Uref

open Util
open Ull

type value = value_ uref
and value_ = 
  | Lit of lit
  | Con of fn * con
  | Var of int
and lit = Int | String | Void
and con = Quote | List
and stack = value ulist
and costack = stack ulist
and fn = costack * costack * Boolean.balg [@@deriving show]

let fresh () = ufresh (), ufresh (), Boolean.bfresh ()
