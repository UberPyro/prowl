open! Batteries

open Maude_ocaml
open Unify

type distack = value seq seq
and 'a seq = 'a seq_t list
and 'a seq_t = 
  | Elem of 'a
  | SeqVar of int
and value = 
  | Int | String
  | List of fn
  | Quote of fn
  | Var of int
and fn = distack * distack * distack

let distack_mod () = parse_mod Distack.prog "DISTACK"
