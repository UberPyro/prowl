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

type ('a, 'b) t = (string, 'a) Ouro.t * 'b uctx [@@deriving show]

let make d = d, (Dict.empty, HDict.create 8)

let add k v = map1 (Ouro.insert k v)
let add_many es = map1 (Ouro.insert_many es)
let find k x = (first %> Ouro.find_rec k
  %> map2 (fun (lazy y) -> y, snd x)) x
let find_opt k x = (first %> Ouro.find_rec_opt k
  %> Option.map (map2 (fun (lazy y) -> y, snd x))) x

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
  HDict.fold Dict.add locals escapees, snd @@ make d
