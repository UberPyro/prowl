open! Batteries
open Uref
(* open Printf *)

open Syntax
(* open Meta *)

exception Kablooey of string
exception DifferentlyHeighted
exception DifferentlyExtant
exception StuntedCostack
exception EmptyStack

type _value = 
  | Int of int
  | String of string
  | Quote of Hir1.expr
  | List of Hir1.expr list
  | Poly
  [@@deriving show]

module Sol = Set.Make(struct
  type t = _value
  include Stdlib
end)

type value = Sol.t uref

type ustack = _ustack uref
and _ustack = 
  | Push of ustack * value
  | Next of ustack
  | Unit

type ucostack = _ucostack uref
and _ucostack = 
  | Real of ustack
  | Fake of ucostack
  | Over of ucostack
  | Void

let select_val v1 v2 = match v1, v2 with
  | Int i1, Int i2 -> 
    if i1 = i2 then Sol.singleton v1
    else Sol.empty
  | String s1, String s2 -> 
    if s1 = s2 then Sol.singleton v1
    else Sol.empty
  | Quote _, Quote _ -> raise @@ Kablooey "quote"
  | List _, List _ -> raise @@ Kablooey "list"
  | _, _ -> Sol.empty

let unify_val = unite ~sel:begin fun x y -> 
  List.cartesian_product (Sol.to_list x) (Sol.to_list y)
  |> List.fold (fun a (e1, e2) -> select_val e1 e2 |> Sol.union a) Sol.empty
end

let rec unify_stack r = unite ~sel:begin fun x y -> match x, y with
  | Push (u1, v1), Push (u2, v2) -> 
    unify_val v1 v2;
    unify_stack u1 u2;
    x
  | Next u1, Next u2 -> 
    unify_stack u1 u2; 
    x
  | (Push _ | Unit as y), Next _ | Next _, (Push _ | Unit as y) -> y
  | Unit, Unit -> Unit
  | Push _, Unit | Unit, Push _ -> raise DifferentlyHeighted
end r

let rec unify_costack r = unite ~sel:begin fun x y -> match x, y with
  | Real u1, Real u2 -> 
    unify_stack u1 u2;
    x
  | Fake u1, Fake u2 -> 
    unify_costack u1 u2;
    x
  | Over u1, Over u2 -> 
    unify_costack u1 u2;
    x
  | (Real _ | Fake _ | Void as y), Over _ 
  | Over _, (Real _ | Fake _ | Void as y) -> y
  | (Real _ | Fake _), Void | Void, (Real _ | Fake _)
  | Real _, Fake _ | Fake _, Real _ -> raise DifferentlyExtant
  | Void, Void -> Void
end r

let iter_costack f = uget %> function
  | Real u -> f u
  | Over _ -> raise StuntedCostack
  | _ -> ()

let (let$) (x, y) f = 
  iter_costack (fun u -> iter_costack (fun v -> f (u, v)) y) x

let pop = uget %> function
  | Push (u, v) -> u, v
  | Next u -> u, uref @@ Sol.singleton Poly
  | Unit -> raise EmptyStack

let pop2 u0 = 
  let u1, v1 = pop u0 in
  let u2, v2 = pop u1 in
  u2, v2, v1

let push u v = uref @@ Push (u, v)
let push2 u v2 v1 = push (push u v2) v1

let map_val f v = uref @@ Sol.map f (uget v)

let expr ((e_, sp) : Hir1.expr) i o = match e_ with
  | `gen -> unify_costack i o
  | `fab -> unify_costack (uref @@ Fake i) o
  | `swap -> 
    let$ (i, o) = i, o in
    let u, v2, v1 = pop2 i in
    unify_stack (push2 u v1 v2) o
  | `unit -> 
    let$ (i, o) = i, o in
    let u, v = pop i in
    let v' = v |> map_val @@ fun x -> 
      Quote begin match x with
        | Int i -> `int i, sp
        | String s -> `string s, sp
        | Quote e -> `quote e, sp
        | List es -> `list es, sp
        | Poly -> `dag (`zap, sp), sp
      end in
    unify_stack (push u v') o
  
  
  | _ -> failwith "todo"
