(* nondeterministic union-find *)
open! Batteries
open Ptmap
open Either
open Tuple2

let (let+) x f = List.map f x
let (let*) x f = List.(map f x |> flatten)

let modify k f = update k @@ function
  | Some x -> Some (f x)
  | None -> raise @@ Invalid_argument "ptmap modify"

type 'a t = 'a puf list
and 'a puf = ('a, int) Either.t Ptmap.t * int Ptmap.t

let rec search x t = match find x @@ fst t with
  | Left v -> x, v
  | Right i -> search i t

let search_all x = List.map (search x)

let merge sel x y puf = 
  let ix, vx = search x puf
  and iy, vy = search y puf in
  let+ v, t = sel vx vy puf in  (* relax in spec? *)
  if ix = iy then t
  else match Stdlib.compare (find x @@ snd t) (find y @@ snd t) with
    | 1 -> map1 (add iy (Right ix) % add ix (Left v)) t
    | -1 -> map1 (add ix (Right iy) % add iy (Left v)) t
    | _ -> map (add ix (Right iy) % add iy (Left v)) (modify iy succ) t

let set_det x v puf = 
  let i, _ = search x puf in
  map1 (add i (Left v)) puf

let update_det x f puf = 
  let i, v = search x puf in
  map1 (add i (Left (f v))) puf

let update_nondet x f puf = 
  let i, v1 = search x puf in
  let+ v2 = f v1 in
  map1 (add i (Left v2)) puf

let (<|>) = (@)
let pure puf = [puf]

let empty = empty, empty
let add_det x v = map (add x (Left v)) (add x 0)
let add_nondet x v = List.map (add_det x v)
