open Batteries

open Type

exception Unbound_variable of string

module Dict = Map.Make(struct
  type t = string
  let compare = compare
end)

type t = (bool * Costack.t * Costack.t) Dict.t

(* let get k e = 
  match Dict.find_opt k e with
  | Some (false, c1, c2) -> c1, c2
  | None -> raise @@ Unbound_variable k *)


