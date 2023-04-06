open! Batteries

open Tuple2
open Uref

let pp_uref fmt x y = fmt x (uget y)

module PP = struct
  type 'a t = (string * 'a) list [@@deriving show]
end

module Dict = struct
  include Map.Make(String)
  let pp h fmt = bindings %> PP.pp h fmt
end

module HDict = struct
  include Hashtbl.Make(String)
  let pp h fmt = to_list %> PP.pp h fmt
end

type 'a uctx = 'a uref Dict.t * 'a uref HDict.t [@@deriving show]

type ('a, 'b) t = 'a Dict.t * 'b uctx [@@deriving show]

let init d = d, (Dict.empty, HDict.create 4)

let add k v = map1 (Dict.add k v)
let add_many es = 
  map1 (fun d -> List.fold (fun a (k, v) -> Dict.add k v a) d es)
let find k = first %> Dict.find k
let find_opt k = first %> Dict.find_opt k

let rec unite unite_val k v (d, (escapees, locals as env)) = 
  match HDict.find_option locals k with
  | Some v' -> unite_val v v'
  | None -> match Dict.find_opt k escapees with
    | Some v' -> 
      HDict.add locals k @@ (uref % uget) v';
      unite unite_val k v (d, env)
    | None -> HDict.add locals k v

let ret k (_, (escapees, locals)) = 
  match HDict.find_option locals k with
  | Some v -> v
  | None -> match Dict.find_opt k escapees with
    | Some v -> v
    | None -> failwith "Unbound unification variable"

let narrow (d, (escapees, locals)) = 
  HDict.fold Dict.add locals escapees, snd @@ init d
