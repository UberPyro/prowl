open! Batteries
open Ptmap
open Either
open Tuple2

let modify k f = update k @@ function
  | Some x -> Some (f x)
  | None -> raise @@ Invalid_argument "ptmap modify"

(* roots produce 'a, children produce int *)
type 'a t = ('a, int) Either.t Ptmap.t * int Ptmap.t

let rec search x t = match find x @@ fst t with
  | Left v -> x, v
  | Right i -> search i t

let merge sel x y t = 
  let ix, vx = search x t
  and iy, vy = search y t in
  if ix = iy then t
  else let new_rep = Left (sel vx vy) in
    match Stdlib.compare (find x @@ snd t) (find y @@ snd t) with
    | 1 -> map1 (add iy (Right ix) % add ix new_rep) t
    | -1 -> map1 (add ix (Right iy) % add iy new_rep) t
    | _ -> map (add ix (Right iy) % add iy new_rep) (modify iy succ) t

let empty = empty, empty
let add x v = map (add x v) (add x 0)
