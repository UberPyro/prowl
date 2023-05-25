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

type 'a uctx = 'a uref Dict.t [@@deriving show]

type ('a, 'b) t = (string, 'a) Ouro.t * 'b uctx [@@deriving show]

let make d = d, Dict.empty

let add k v = map1 (Ouro.insert k v)
let add_many es = map1 (Ouro.insert_many es)
let find k x = (first %> Ouro.find_rec k
  %> map2 (fun (lazy y) -> y, snd x)) x
let find_opt k x = (first %> Ouro.find_rec_opt k
  %> Option.map (map2 (fun (lazy y) -> y, snd x))) x

let return k (_, uctx) = Dict.find k uctx

let init poly k (o, uctx) = o, Dict.add k poly uctx
let init_many poly ks (o, uctx) = 
  o, Dict.add_seq (Seq.of_list @@ 
    List.map (fun x -> x, poly) ks) uctx
