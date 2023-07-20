open! Batteries
open Printf
open Util
open System

open Tuple3

type t = 
    (string, bool * Fn.t) Ouro.t
  * (string, Value.t) Map.t
  * (string, Stack.t) Map.t

let empty : t = Ouro.empty, Map.empty, Map.empty

let find_rec_opt k t = Ouro.find_rec_opt k (first t)
let insert_many lst t = map1 (Ouro.insert_many lst) t
let insert k v t = map1 (Ouro.insert k v) t
let vmap f t = map1 (Ouro.vmap f) t

let unify_uvar n u t = 
  Value.unify u @@ Map.find n @@ second t

let unify_ustack n u t = 
  Stack.unify u @@ Map.find n @@ third t

let introduce_uvar n u t = 
  map2 (Map.add n u) t

let introduce_stkvar n u t = 
  map3 (Map.add n u) t

let pretty_ouro out t = 
  let lst = Ouro.to_list (first t) in
  fprintf out "Context: \n";
  lst |> List.iter @@ fun (n, (_, fn)) -> 
    fprintf out "%s => " n;
    Fn.pretty out fn;
    fprintf out "%s" "\n"

let pretty_uctx out t = 
  let lst = Map.to_seq (second t) |> List.of_seq in
  fprintf out "Unification Variables: \n";
  lst |> List.iter @@ fun (n, uvar) -> 
    fprintf out "%s -> " n;
    Value.pretty out uvar;
    fprintf out "%s" "\n"

let pretty_stkctx out t = 
  let lst = Map.to_seq (third t) |> List.of_seq in
  fprintf out "Stack Variables: \n";
  lst |> List.iter @@ fun (n, uvar) -> 
    fprintf out "%s -> " n;
    Stack.pretty out uvar;
    fprintf out "%s" "\n"

let pretty out t = 
  pretty_ouro out t;
  fprintf out "%s" "\n";
  pretty_uctx out t;
  fprintf out "%s" "\n";
  pretty_stkctx out t
