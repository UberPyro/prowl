open! Batteries
open Tuple2
open Map

type 'a t = ('a, 'a) Map.t * ('a, int) Map.t

let parent x = find x % fst
let rank x = find x % snd

let rec search x t = 
  let p = parent x t in
  if p = x then x
  else search p t

let merge x y t = 
  let rx = search x t
  and ry = search y t in
  if rx = ry then t
  else match Stdlib.compare (rank x t) (rank y t) with
  | 1 -> map1 (add ry rx) t
  | -1 -> map1 (add rx ry) t
  | _ -> Tuple2.map (add rx ry) (modify ry succ) t

let empty = empty, empty
let add x = Tuple2.map (add x x) (add x 0)
