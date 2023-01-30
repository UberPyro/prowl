open! Batteries
open Type

exception Unbound_variable of string
module Dict = Map.Make(String)
module HashDict = Hashtbl.Make(String)

module E : sig
  type t

  val empty : t
  val get : string -> t -> costack * costack
  val set : string -> costack * costack -> t -> t
  val promote : string -> t -> t
end = struct

  type t = (gen * costack * costack) Dict.t
  and gen = General | Special

  let empty = Dict.empty

  let get k e = 
    match Dict.find_opt k e with
    | Some (Special, c1, c2) -> c1, c2
    | Some (General, c1, c2) -> 
      let r = refresh () in
      r#costack c1, r#costack c2
    | None -> raise @@ Unbound_variable k

  let set k (c1, c2) = Dict.add k (Special, c1, c2)
  let promote k e = 
    e |> Dict.add k @@ begin fun (_, c1, c2) -> 
      General, c1, c2
    end @@ Dict.find k e

end

module type VARIABLE = sig
  type t
  val unite : t -> t -> unit
  val freshen : t -> t
end

module Envelop(V : VARIABLE) : sig
  type t

  val create : unit -> t
  val unite : string -> V.t -> t -> unit
  val ret : string -> t -> V.t
  val narrow : t -> t
end = struct
  type t = V.t Dict.t * V.t HashDict.t

  let create () = Dict.empty, HashDict.create 4

  let rec unite k v (escapees, locals as env) = 
    match HashDict.find_option locals k with
      | Some v' -> V.unite v v'
      | None -> match Dict.find_opt k escapees with
        | Some v' -> 
          HashDict.add locals k @@ V.freshen v';
          unite k v env
        | None -> HashDict.add locals k v

  let ret k (escapees, locals) = match HashDict.find_option locals k with
    | Some v -> v
    | None -> match Dict.find_opt k escapees with
      | Some v -> v
      | None -> failwith "Unbound unification variable"
  
  let narrow (escapees, locals) = 
    HashDict.fold Dict.add locals escapees, snd @@ create ()

end

module UEnv = Envelop(struct
  type t = var
  let unite = unify
  let freshen = (refresh ())#var
end)

module StackEnv = Envelop(struct
  type t = var seq
  let unite = Ulist.unite_seq ~sel:unify
  let freshen = (refresh ())#stack
end)

module CostackEnv = Envelop(struct
  type t = var seq seq
  let unite = unify_costack
  let freshen = (refresh ())#costack
end)

module TypeEnv : sig
  type t

  val empty : t
  val get : string -> t -> var list list
  val set : string -> var list list -> t -> t
end = struct
  type t = var list list Dict.t

  let empty = Dict.empty
  let get = Dict.find
  let set = Dict.add
end

open Tuple5

type t = E.t * UEnv.t * StackEnv.t * CostackEnv.t * TypeEnv.t

let create () = 
  E.empty, UEnv.create (), StackEnv.create (), CostackEnv.create (), TypeEnv.empty

let narrow = 
  map2 UEnv.narrow
  %> map3 StackEnv.narrow
  %> map4 CostackEnv.narrow

let get s = first %> E.get s
let set k v = map1 @@ E.set k v
let promote k = map1 (E.promote k)

let unite s v = second %> UEnv.unite s v
let ret s = second %> UEnv.ret s

let unite_stack s v = third %> StackEnv.unite s v
let ret_stack s = third %> StackEnv.ret s

let unite_costack s v = fourth %> CostackEnv.unite s v
let ret_costack s = fourth %> CostackEnv.ret s

let get_type s = fifth %> TypeEnv.get s
let set_type k v = map5 @@ TypeEnv.set k v
