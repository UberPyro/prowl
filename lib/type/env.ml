open! Batteries
open Type

exception Unbound_variable of string
module Dict = Map.Make(String)

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

module UEnv : sig
  type t

  val empty : t
  val init : string -> t -> t
  val unite : string -> var -> t -> unit
  val ret : string -> t -> var
end = struct

  type t = var Dict.t

  let empty = Dict.empty

  (* idempotent *)
  let init k e = 
    if Dict.mem k e then e
    else Dict.add k (fresh_var ()) e

  let unite k v e = unify (Dict.find k e) v

  let ret k e = Dict.find k e

end

open Tuple2

type t = E.t * UEnv.t

let empty = E.empty, UEnv.empty

let get s = fst %> E.get s
let set k v = map1 (E.set k v)
let promote k = map1 (E.promote k)

let init s = map2 (UEnv.init s)
let unite s v = snd %> UEnv.unite s v
let ret s = snd %> UEnv.ret s
