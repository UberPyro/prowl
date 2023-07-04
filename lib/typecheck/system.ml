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
  | Var of int  (* physical *)
and fn = distack * distack * distack

type tyst = tysum Nuf.t
and tysum = 
  | C of distack
  | S of value seq
  | V of value

let[@warning "-8"] get_distack (C d) = d
let[@warning "-8"] get_stack (S s) = s
let[@warning "-8"] get_value (V v) = v

(* note: always runs... *)
let distack_mod = parse_mod Distack.prog "DISTACK"
let symap = mk_symmap distack_mod

let parse_var s = String.sub s 1 (String.length s - 1) |> String.to_int
let pretty_value_var = Int.to_string %> (^) "V"
let pretty_stack_var = Int.to_string %> (^) "S"
let pretty_costack_var = Int.to_string %> (^) "C"

let rec distack_to_term d = 
  List.fold_left begin fun t -> function
    | Elem s -> build_op "stack" symap [t; stack_to_term s]
    | SeqVar i -> build_var (pretty_costack_var i) "C"
  end (build_op "void" symap []) d

and stack_to_term s = 
  List.fold_left begin fun t -> function
    | Elem v -> build_op "value" symap [t; value_to_term v]
    | SeqVar i -> build_var (pretty_stack_var i) "S"
  end (build_op "empty" symap []) s

and value_to_term = function
  | Int -> build_op "int" symap []
  | String -> build_op "str" symap []
  | List fn -> build_op "lst" symap [fn_to_term fn]
  | Quote fn -> build_op "quo" symap [fn_to_term fn]
  | Var i -> build_var (pretty_value_var i) "V"

and fn_to_term t = 
  let h, j, k = Tuple3.mapn distack_to_term t in
  build_op "fn" symap [h; j; k]

let term_to_distack _ _ = failwith "todo"
and term_to_stack _ _ = failwith "todo"
and term_to_value _ _ = failwith "todo"

(* let rec unify_distack d1 d2 s = 
  let+ t = unify distack_mod (distack_to_term d1) (distack_to_term d2) in
  sub_fold s t

and unify_stack s1 s2 s = 
  let+ t = unify distack_mod (stack_to_term s1) (stack_to_term s2) in
  sub_fold s t

and unify_value v1 v2 s = 
  let+ t = unify distack_mod (value_to_term v1) (value_to_term v2) in
  sub_fold s t

and sub_fold tyst = 
  List.fold_left (fun tyst_acc (var, term) -> sub var term tyst_acc) tyst

and sub var term tyst = match term_sort term with
  | "C" -> 
    let distack, tyst_linked = term_to_distack term tyst
    and costack_var = parse_var var in
    Puf.set costack_var distack tyst_linked
  | "S" -> 
    let stack, tyst_linked = term_to_stack term tyst
    and stack_var = parse_var var in
    Puf.set stack_var stack tyst_linked
  | "V" -> 
    let value, tyst_linked = term_to_value term tyst
    and value_var = parse_var var in
    Puf.set value_var value tyst_linked
  | s -> 
    let msg = Printf.sprintf "System.sub : Unrecognized sort [%s]" s in
    raise @@ Invalid_argument msg *)

(* let rec unify_distack d1 d2 puf0 = 
  Nuf.merge begin fun c1 c2 puf ->
    subs (unify distack_mod c1 c2) puf
  end d1 d2 puf0 *)
