open! Batteries
open Uref

open Unify
open Ulist

module ModeLit = struct
  type t = (bool * bool) * (bool * bool) [@@deriving show]
  let (+) b1 b2 = (b1 || b2) && not (b1 && b2)
  let lift_mode f ((a1, b1), (c1, d1)) ((a2, b2), (c2, d2)) = 
    (f a1 a2, f b1 b2), (f c1 c2, f d1 d2)
  let xor_const = lift_mode (+)
  let and_const = lift_mode (&&)
  let zero = (false, false), (false, false)
  let one = (true, true), (true, true)
  let det_to_string = function
    | false, false -> "fn"
    | true, false -> "pt"
    | false, true -> "mt"
    | true, true -> "rl"
  let to_string bs = 
    let b1, b2 = Tuple2.mapn det_to_string bs in
    Printf.sprintf "%s %s" b1 b2
end

module Umode = Ubool.Make(ModeLit)
open Umode

type value = value_ uref
and value_ = 
  | Lit of lit
  | Con of fn * con
  | Var of int
and lit = Int | String | Void
and con = Quote | List
and stack = value ulist
and costack = stack ulist
and fn = costack * costack * ubool [@@deriving show]

let fresh () = ufresh (), ufresh (), bfresh ()
