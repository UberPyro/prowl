open! Batteries
open Uref

open Util
open Ull

open Boolean

type mode = ((bool * bool) * (bool * bool)) boolean [@@deriving show]

type value = value_ uref
and value_ = 
  | Lit of lit
  | Con of fn * con
  | Var of int
and lit = Int | String | Void
and con = Quote | List
and stack = value ulist
and costack = stack ulist
and fn = costack * costack * mode [@@deriving show]

let fresh () = ufresh (), ufresh (), bfresh ()
