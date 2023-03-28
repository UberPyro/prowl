open! Batteries
open Uref
(* open Printf *)

open Syntax
(* open Meta *)

let pp_uref fmt x y = fmt x (uget y)

let c = ref (-1)
let fresh () = 
  incr c;
  !c

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
  | Free of int

and value = _value_poly uref [@@deriving show]

and context = Hir1.expr Dict.t

type ustack = _ustack uref
and _ustack = 
  | Push of ustack * value
  | Next of int
  | Unit

type ucostack = _ucostack uref
and _ucostack = 
  | Real of ustack
  | Fake of ucostack
  | Quasi of ucostack * ustack
  | Over of int

exception Kablooey of string
exception DifferentlyHeighted
exception DifferentlyExtant
exception EmptyStack
exception DifferentlyTyped of _value * _value
exception Noncallable of _value
exception Polycall
exception Polyelimination
exception UnboundVariable of string

let fresh_val () = uref @@ Free (fresh ())
let fresh_stack () = uref @@ Next (fresh ())
let fresh_costack () = uref @@ Over (fresh ())

let mk_val s = uref @@ Bound (Set.singleton s)

let fold_val f x0 y0 = match x0, y0 with
  | Bound _ as b, Free _ | Free _, b -> b
  | Bound x, Bound y -> Bound begin
    List.cartesian_product (Set.to_list x) (Set.to_list y)
    |> List.fold (fun a (e1, e2) -> f e1 e2 |> Set.union a) Set.empty
  end

let rec select_val v1 v2 = match v1, v2 with
  | `int i1, `int i2 -> 
    if i1 = i2 then Set.singleton v1
    else Set.empty
  | `string s1, `string s2 -> 
    if s1 = s2 then Set.singleton v1
    else Set.empty
  | `closedValue v1', `closedValue v2' -> 
    let v' = uref @@ fold_val select_val (uget v1') (uget v2') in
    Set.singleton @@ `closedValue v'
  | `closure _, `closure _
  | `closedList _, `closedList _ -> Set.of_list [v1; v2]
  | _v1, _v2 -> raise @@ DifferentlyTyped (_v1, _v2)

let unify_val = unite ~sel:(fold_val select_val)

let rec unify_stack r = unite ~sel:begin fun x y -> match x, y with
  | Push (u1, v1), Push (u2, v2) -> 
    unify_val v1 v2;
    unify_stack u1 u2;
    x
  | Next _, Next _ -> x
  | (Push _ | Unit as y), Next _ | Next _, (Push _ | Unit as y) -> y
  | Unit, Unit -> Unit
  | Push _, Unit | Unit, Push _ -> raise DifferentlyHeighted
end r

let rec unify_costack r = unite ~sel:begin fun x y -> match x, y with
  | Real u1, Real u2 -> 
    unify_stack u1 u2;
    x
  | Quasi (_, u1), (Real u2 as r) | (Real u1 as r), Quasi (_, u2) -> 
    unify_stack u1 u2;
    r
  | Fake u1, Fake u2 -> 
    unify_costack u1 u2;
    x
  | Quasi (u1, _), (Fake u2 as f) | (Fake u1 as f), Quasi (u2, _) -> 
    unify_costack u1 u2;
    f
  | Quasi (c1, s1), Quasi (c2, s2) -> 
    unify_costack c1 c2;
    unify_stack s1 s2;
    x
  | Over _, Over _ -> x
  | (Real _ | Fake _ | Quasi _ as y), Over _ 
  | Over _, (Real _ | Fake _ | Quasi _ as y) -> y
  | Real _, Fake _ | Fake _, Real _ -> raise DifferentlyExtant
end r

type ('a, 'b) ior = L of 'a | R of 'b | B of 'a * 'b

let copop u0 = match uget u0 with
  | Real u -> R u
  | Fake u -> L u
  | Quasi (c, r) -> B (c, r)
  | Over _ -> B (u0, fresh_stack ())

let real u = uref @@ Real u
let fake u = uref @@ Fake u
let quasi c s = uref @@ Quasi (c, s)

let iter_costack f c = match copop c with
  | R u -> f u
  | B (_, u) -> f u
  | L _ -> ()

let (let$) (x, y) f = 
  iter_costack (fun u -> iter_costack (fun v -> f (u, v)) y) x

let pop u0 = match uget u0 with
  | Push (u, v) -> u, v
  | Next _ -> u0, fresh_val ()
  | Unit -> raise EmptyStack

let pop2 u0 = 
  let u1, v1 = pop u0 in
  let u2, v2 = pop u1 in
  u2, v2, v1

let push u v = uref @@ Push (u, v)
let push2 u v2 v1 = push (push u v2) v1

let iter_val f_bound f_free v = match uget v with
  | Bound s -> Set.iter f_bound s
  | Free i -> f_free i

let rec expr ctx ((e_, _) : Hir1.expr) i o = match e_ with
  | `gen -> begin match copop i with
    | R _ -> unify_costack i o
    | L _ -> unify_costack (fake i) o
    | B (c, s) -> unify_costack (quasi (fake c) s) o
  end
  | `fab -> unify_costack (uref @@ Fake i) o
  | `elim -> begin match copop i with
    | R _ -> unify_costack i o
    | L c -> unify_costack c o
    | B _ -> raise Polyelimination
  end
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
    end (fun _ -> raise Polycall)
  | `zap -> 
    let$ (i, o) = i, o in
    let u, _ = pop i in
    unify_stack u o
  
  | #common as c -> 
    let$ (i, o) = i, o in
    unify_stack (push i @@ mk_val c) o
  | `id s -> call ctx s i o
  | `jux es -> 
    let rec go i' o' = function
      | e1 :: (_ :: _ as es) -> 
        let c = fresh_costack () in
        expr ctx e1 i' c;
        go c o' es
      | [e] -> expr ctx e i' o'
      | [] -> unify_costack i' o' in
    go i o es
  
  | _ -> failwith "todo"

and call ctx s i o = match Dict.find_opt s ctx with
  | Some e -> expr ctx e i o
  | None -> raise @@ UnboundVariable s
