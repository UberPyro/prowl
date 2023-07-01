open! Batteries
(* open Tuple3 *)

open Maude_ocaml
open Unify

open Util

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

type tyst = (distack Puf.t * value seq Puf.t * value Puf.t) list

let (let*) x f = List.map f x |> List.flatten
let (let+) x f = List.map f x

(* note: always runs... *)
let distack_mod = parse_mod Distack.prog "DISTACK"
let symap = mk_symmap distack_mod

let parse_var s = String.sub s 1 (String.length s - 1) |> String.to_int
let pretty_value_var = Int.to_string %> (^) "V"
let pretty_stack_var = Int.to_string %> (^) "S"
let pretty_costack_var = Int.to_string %> (^) "C"

(* let unify_distack d1 d2 s = 
  let* t = unify distack_mod (distack_to_term d1) (distack_to_term d2) in
  List.fold_left begin fun s_acc (var, term) -> 
    match term_sort term with
    | "C" -> 
      let distack, s_acc_2 = term_to_distack term s_acc
      and costack_var = parse_var var in
      Puf.get costack_var
  end s t *)

(* parse & print variable names - 12 <-> C12  [done] *)
(* terms have variables! - deconstruction *)
