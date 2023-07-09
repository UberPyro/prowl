open! Batteries
open Uref

open Util
open Ull

type value = value_ uref
and value_ = 
  | Lit of lit
  | Con of fn * con
and lit = Int | String
and con = Quote | List
and stack = value ulist
and costack = stack ulist
and fn = costack * costack
