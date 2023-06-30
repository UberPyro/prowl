open! Batteries
open Ptmap
open Tuple3

exception MergeError

let modify k f = update k @@ function
  | Some x -> Some (f x)
  | None -> raise @@ Invalid_argument "ptmap modify"

let move k1 k2 t = match find_opt k1 t with
  | None -> t
  | s -> t |> begin update k2 @@ function
    | Some _ -> raise MergeError
    | None -> s
  end |> remove k1

type 'a t = int Ptmap.t * int Ptmap.t * 'a Ptmap.t

let parent x = find x % first
let rank x = find x % second

let rec search x t = 
  let p = parent x t in
  if p = x then x
  else search p t

let merge x y t = 
  let rx = search x t
  and ry = search y t in
  if rx = ry then t
  else match Stdlib.compare (rank x t) (rank y t) with
  | 1 -> map (add ry rx) Fun.id (move ry rx) t
  | -1 -> map (add rx ry) Fun.id (move rx ry) t
  | _ -> map (add rx ry) (modify ry succ) (move rx ry) t

let empty = empty, empty, empty
let add x = map (add x x) (add x 0) Fun.id

let get x t = third t |> Ptmap.find_opt (search x t)
let set x y t = 
  t |> map3 @@ Ptmap.update (search x t) begin function
    | Some _ -> raise MergeError
    | None -> Some y
  end
