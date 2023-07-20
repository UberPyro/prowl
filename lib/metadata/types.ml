open! Batteries
open Unify
open Ulist

type value = (string, fn) Ueither.t Usyn.t
and stack = value Ulist.t
and costack = stack Ulist.t
and fn = Fn of (costack * costack)
[@@deriving show]

let fresh () = Fn (ufresh (), ufresh ())
