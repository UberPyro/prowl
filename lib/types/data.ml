open! Batteries
open Uref
open Ull

type 'a node = ('a * 'a delta array) uref
and 'a delta = {
  down: 'a node array;
  bot: 'a node;
  up: 'a node array;
}

and costack = stack ulist
and stack = value ulist
and value = _value ulist
and _value = 
  | Lit of lit
  | Con of con * fn
  | Var of int

and lit = 
  | Int
  | String

and con = 
  | Quote
  | List

and fn = costack delta
