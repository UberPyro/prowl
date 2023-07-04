open! Batteries
open Either
open Printf
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

(* note: always runs... *)
let distack_mod = parse_mod Distack.prog "DISTACK"
let symap = mk_symmap distack_mod

let parse_var s = String.sub s 1 (String.length s - 1) |> String.to_int
let pretty_value_var = sprintf "V%d:V"
let pretty_stack_var = sprintf "S%d:S"
let pretty_costack_var = sprintf "C%d:C"

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

let create_tracker () = Hashtbl.create 16

let process wrap m puf k = match Hashtbl.find_option m k with
  | Some v -> v, puf
  | None -> 
    let nu = unique () in
    Hashtbl.add m k nu;
    nu, Nuf.add_det nu (wrap nu) puf

let process_distack m = process (fun x -> C [SeqVar x]) m
let process_stack m = process (fun x -> S [SeqVar x]) m
let process_value m = process (fun x -> V (Var x)) m

let rec term_to_distack tbl term tyst = match break term with
  | Left s -> 
    let i, puf = process_distack tbl tyst s in
    [SeqVar i], puf
  | Right ("stack", [s]) -> 
    let stack, linked = term_to_stack tbl s tyst in
    [Elem stack], linked
  | Right ("+", [d1; d2]) -> 
    let distack1, linked1 = term_to_distack tbl d2 tyst in
    let distack2, linked2 = term_to_distack tbl d1 linked1 in
    distack1 @ distack2, linked2
  | Right (s, lst) -> 
    sprintf "term_to_distack: Operator [%s] with %d arguments" s (List.length lst)
    |> failwith
and term_to_stack tbl term tyst = match break term with
  | Left s -> 
    let i, puf = process_stack tbl tyst s in
    [SeqVar i], puf
  | Right ("value", [v]) -> 
    let value, linked = term_to_value tbl v tyst in
    [Elem value], linked
  | Right ("*", [s1; s2]) -> 
    let stack1, linked1 = term_to_stack tbl s2 tyst in
    let stack2, linked2 = term_to_stack tbl s1 linked1 in
    stack1 @ stack2, linked2
  | Right (s, lst) -> 
    sprintf "term_to_stack: Operator [%s] with %d arguments" s (List.length lst)
    |> failwith
and term_to_value tbl term tyst = match break term with
  | Left v -> 
    let i, puf = process_value tbl tyst v in
    Var i, puf
  | Right ("int", []) -> Int, tyst
  | Right ("str", []) -> String, tyst
  | Right ("lst", [fn]) -> 
    let x, puf = term_to_fn tbl fn tyst in
    List x, puf
  | Right ("quo", [fn]) -> 
    let x, puf = term_to_fn tbl fn tyst in
    Quote x, puf
  | Right (s, lst) -> 
    sprintf "term_to_value: Operator [%s] with %d arguments" s (List.length lst)
    |> failwith

and term_to_fn tbl term tyst = match break term with
  | Left _ -> failwith "term_to_fn: variable"
  | Right ("fn", [d1; d2; d3]) -> 
    let c1, l1 = term_to_distack tbl d1 tyst in
    let c2, l2 = term_to_distack tbl d2 l1 in
    let c3, l3 = term_to_distack tbl d3 l2 in
    (c1, c2, c3), l3
  | Right (s, lst) -> 
    sprintf "term_to_fn: Operator [%s] with %d arguments" s (List.length lst)
    |> failwith

let sub tbl var term tyst = match term_sort term with
  | "C" -> 
    let distack, tyst_linked = term_to_distack tbl term tyst
    and costack_var = parse_var var in
    Nuf.set_det costack_var (C distack) tyst_linked
  | "S" -> 
    let stack, tyst_linked = term_to_stack tbl term tyst
    and stack_var = parse_var var in
    Nuf.set_det stack_var (S stack) tyst_linked
  | "V" -> 
    let value, tyst_linked = term_to_value tbl term tyst
    and value_var = parse_var var in
    Nuf.set_det value_var (V value) tyst_linked
  | s -> 
    let msg = Printf.sprintf "System.sub : Unrecognized sort [%s]" s in
    raise @@ Invalid_argument msg

let sub_all tyst = 
  let tbl = create_tracker () in
  List.fold_left (fun tyst_acc (var, term) -> sub tbl var term tyst_acc) tyst

let unify_distack k = Nuf.merge begin fun[@warning "-8"] (C d1 as c) (C d2) puf -> 
  unify distack_mod (distack_to_term d1) (distack_to_term d2)
  |> List.map (fun terms -> c, sub_all puf terms)
end k

let unify_stack k = Nuf.merge begin fun[@warning "-8"] (S s1 as s) (S s2) puf -> 
  unify distack_mod (stack_to_term s1) (stack_to_term s2)
  |> List.map (fun terms -> s, sub_all puf terms)
end k

let unify_value k = Nuf.merge begin fun[@warning "-8"] (V v1 as v) (V v2) puf -> 
  unify distack_mod (value_to_term v1) (value_to_term v2)
  |> List.map (fun terms -> v, sub_all puf terms)
end k

let mk_poly_value = 
  let nu = unique () in
  let v = Var nu in
  v, Nuf.add_det nu (V v)

let mk_empty_stack puf = 
  let nu = unique () in
  let stk = [SeqVar nu] in
  stk, Nuf.add_det nu (S stk) puf

let mk_unit_distack puf = 
  let stk, puf' = mk_empty_stack puf in
  let nu = unique () in
  let distk = [Elem stk] in
  distk, Nuf.add_det nu (C distk) puf'

let mk_polyfn p0 = 
  let d1, p1 = mk_unit_distack p0 in
  let d2, p2 = mk_unit_distack p1 in
  let d3, p3 = mk_unit_distack p2 in
  (d1, d2, d3), p3

let mk_endofn p0 = 
  let d1, p1 = mk_unit_distack p0 in
  let d2, p2 = mk_unit_distack p1 in
  (d1, d2, d1), p2

let mk_no_op p0 = 
  let d1, p1 = mk_unit_distack p0 in
  (d1, d1, d1), p1

let wrap_stack stack p0 = 
  let nu = unique () in
  let distk = [Elem stack] in
  distk, Nuf.add_det nu (C distk) p0
