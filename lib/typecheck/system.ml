open! Batteries
open Either
open Printf

open Maude_ocaml
open Unify

open Util

type comb = value seq seq
and 'a seq = 'a seq_t list
and 'a seq_t = 
  | Elem of 'a
  | SeqVar of int
and value = 
  | Int | String
  | List of fn
  | Quote of fn
  | Var of int  (* physical *)
and fn = comb * comb

type tyst = tysum Nuf.t
and tysum = 
  | C of comb
  | S of value seq
  | V of value

(* note: always runs... *)
let comb_mod = parse_mod Distack.prog "DISTACK"
let symap = mk_symmap comb_mod

let parse_var s = String.sub s 1 (String.length s - 1) |> String.to_int
let pretty_value_var = sprintf "V%d:V"
let pretty_stack_var = sprintf "S%d:S"
let pretty_costack_var = sprintf "C%d:C"

let rec comb_to_term d = 
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
  let u, v = Tuple2.mapn comb_to_term t in
  build_op "fn" symap [u; v]

let create_tracker () = Hashtbl.create 16

let process wrap m puf k = match Hashtbl.find_option m k with
  | Some v -> v, puf
  | None -> 
    let nu = unique () in
    Hashtbl.add m k nu;
    nu, Nuf.add_det nu (wrap nu) puf

let process_comb m = process (fun x -> C [SeqVar x]) m
let process_stack m = process (fun x -> S [SeqVar x]) m
let process_value m = process (fun x -> V (Var x)) m

let rec term_to_comb tbl term tyst = match break term with
  | Left s -> 
    let i, puf = process_comb tbl tyst s in
    [SeqVar i], puf
  | Right ("stack", [s]) -> 
    let stack, linked = term_to_stack tbl s tyst in
    [Elem stack], linked
  | Right ("+", [d1; d2]) -> 
    let comb1, linked1 = term_to_comb tbl d2 tyst in
    let comb2, linked2 = term_to_comb tbl d1 linked1 in
    comb1 @ comb2, linked2
  | Right (s, lst) -> 
    sprintf "term_to_comb: Operator [%s] with %d arguments" s (List.length lst)
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
  | Right ("fn", [d1; d2]) -> 
    let c1, l1 = term_to_comb tbl d1 tyst in
    let c2, l2 = term_to_comb tbl d2 l1 in
    (c1, c2), l2
  | Right (s, lst) -> 
    sprintf "term_to_fn: Operator [%s] with %d arguments" s (List.length lst)
    |> failwith

let sub tbl var term tyst = match term_sort term with
  | "C" -> 
    let comb, tyst_linked = term_to_comb tbl term tyst
    and costack_var = parse_var var in
    Nuf.set_det costack_var (C comb) tyst_linked
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

let sub_all tbl tyst = 
  List.fold_left (fun tyst_acc (var, term) -> sub tbl var term tyst_acc) tyst

let unify_comb k = Nuf.merge begin fun[@warning "-8"] (C d1) (C d2) puf -> 
  let tbl = create_tracker () in
  unify comb_mod (comb_to_term d1) (comb_to_term d2)
  |> List.map @@ fun (c0, terms) -> 
    let p0 = sub_all tbl puf terms in
    let c1, p1 = term_to_comb tbl c0 p0 in
    C c1, p1
end k

let unify_stack k = Nuf.merge begin fun[@warning "-8"] (S s1) (S s2) puf -> 
  let tbl = create_tracker () in
  unify comb_mod (stack_to_term s1) (stack_to_term s2)
  |> List.map @@ fun (s0, terms) -> 
    let p0 = sub_all tbl puf terms in
    let s1, p1 = term_to_stack tbl s0 p0 in
    S s1, p1
end k

let unify_value k = Nuf.merge begin fun[@warning "-8"] (V v1) (V v2) puf -> 
  let tbl = create_tracker () in
  unify comb_mod (value_to_term v1) (value_to_term v2)
  |> List.map @@ fun (v0, terms) -> 
    let p0 = sub_all tbl puf terms in
    let v1, p1 = term_to_value tbl v0 p0 in
    V v1, p1
end k
