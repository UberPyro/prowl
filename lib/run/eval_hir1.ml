open! Batteries
open Uref
(* open Printf *)

open Syntax
(* open Meta *)

let pp_uref fmt x y = fmt x (uget y)

module Set = struct
  include Set
  module PP = struct
    type 'a t = 'a list [@@deriving show]
  end
  let pp f fmt = elements %> PP.pp f fmt
end

module Dict = struct
  include Map.Make(String)
  module D = struct
    type 'a t = (string * 'a) list [@@deriving show]
  end
  let pp h fmt = bindings %> D.pp h fmt
end

type common = [
  | `int of int
  | `string of string
] [@@deriving show]

type _value = [
  common
  | `closure of Hir1.expr * context
  | `closedList of (Hir1.expr * context) list
  | `closedValue of value
] [@@deriving show]

and _value_poly = 
  | Bound of _value Set.t
  | Free

and value = _value_poly uref [@@deriving show]

and context = Hir1.expr Dict.t

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

exception Kablooey of string
exception DifferentlyHeighted
exception DifferentlyExtant
exception StuntedCostack
exception EmptyStack
exception DifferentlyTyped of _value * _value
exception Noncallable of _value
exception Polycall
exception UnboundVariable of string

let mk_val s = uref @@ Bound (Set.singleton s)

let select_val v1 v2 = match v1, v2 with
  | `int i1, `int i2 -> 
    if i1 = i2 then Set.singleton v1
    else Set.empty
  | `string s1, `string s2 -> 
    if s1 = s2 then Set.singleton v1
    else Set.empty
  | `closure _, `closure _ -> raise @@ Kablooey "quote"
  | `closedList _, `closedList _ -> raise @@ Kablooey "list"
  | _v1, _v2 -> raise @@ DifferentlyTyped (_v1, _v2)

let unify_val = unite ~sel:begin fun x0 y0 -> match x0, y0 with
  | Bound _ as b, Free | Free, b -> b
  | Bound x, Bound y -> Bound begin
    List.cartesian_product (Set.to_list x) (Set.to_list y)
    |> List.fold (fun a (e1, e2) -> select_val e1 e2 |> Set.union a) Set.empty
  end
  
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
  | Next u -> u, uref Free
  | Unit -> raise EmptyStack

let pop2 u0 = 
  let u1, v1 = pop u0 in
  let u2, v2 = pop u1 in
  u2, v2, v1

let push u v = uref @@ Push (u, v)
let push2 u v2 v1 = push (push u v2) v1

let iter_val f_bound f_free v = match uget v with
  | Bound s -> Set.iter f_bound s
  | Free -> f_free ()

let rec expr ctx ((e_, sp) : Hir1.expr) i o = match e_ with
  | `gen -> unify_costack i o
  | `fab -> unify_costack (uref @@ Fake i) o
  | `swap -> 
    let$ (i, o) = i, o in
    let u, v2, v1 = pop2 i in
    unify_stack (push2 u v1 v2) o
  | `unit -> 
    let$ (i, o) = i, o in
    let u, v = pop i in
    unify_stack (push u @@ mk_val @@ `closedValue v) o
  | `call -> 
    let$ (i', o') = i, o in
    let u, v = pop i' in
    v |> iter_val begin function
      | `closure (e, ctx') -> expr ctx' e (uref @@ Real u) o
      | `closedValue v -> unify_stack (push i' v) o'
      | _v -> raise @@ Noncallable _v
    end (fun () -> raise Polycall)
  | `zap -> 
    let$ (i, o) = i, o in
    let u, _ = pop i in
    unify_stack u o
  
  | #common as c -> 
    let$ (i, o) = i, o in
    unify_stack (push i @@ mk_val c) o
  | `id s -> call ctx s i o
  (* | `jux es -> 
    let rec go ctx' i' o' = begin function
      | e1 :: e2 :: es -> 

    end *)
  
  | _ -> failwith "todo"

and call ctx s i o = match Dict.find_opt s ctx with
  | Some e -> expr ctx e i o
  | None -> raise @@ UnboundVariable s
