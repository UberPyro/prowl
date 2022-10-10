open Batteries

open Type

exception Unbound_variable of string

module Dict = Map.Make(struct
  type t = string
  let compare = compare
end)

type t = (gen * Costack.t * Costack.t) Dict.t
and gen = General | Special

let empty = Dict.empty

let get k e = 
  match Dict.find_opt k e with
  | Some (Special, c1, c2) -> c1, c2
  | Some (General, c1, c2) -> 
    let refresh = Costack.refresh @@ HT.create 8 in
    refresh c1, refresh c2
  | None -> raise @@ Unbound_variable k

let set k (c1, c2) = Dict.add k (Special, c1, c2)
let save k (c1, c2) = Dict.add k (General, c1, c2)
let promote k e = 
  e |> Dict.add k @@ begin fun (_, c1, c2) -> 
    General, c1, c2
  end @@ Dict.find k e
