(* http://fhur.github.io/data-structure/clojure/2016/04/01/implementing-immutable-union-find-with-clojure.html *)
open! Batteries
open Map

type 'a t = {
  parent_map : ('a, 'a) Map.t;
  rank_map : ('a, int) Map.t;
}

let parent x t = find x t.parent_map
let rank x t = find x t.rank_map

let rec search x t = 
  let p = parent x t in
  if Stdlib.compare p x = 0 then x
  else search p t

let merge x y t = 
  let px = search x t in
  let py = search y t in
  if Stdlib.compare px py = 0 then t
  else match Stdlib.compare (rank x t) (rank y t) with
  | 1 -> {t with parent_map=add py px t.parent_map}
  | -1 -> {t with parent_map=add px py t.parent_map}
  | _ -> {
    parent_map = add px py t.parent_map;
    rank_map = update_stdlib py 
      (fun[@warning "-8"] (Some i) -> Some (i + 1)) t.rank_map;
  }

let empty = {parent_map=empty; rank_map=empty}
let add x t = {
  parent_map=Map.add x x t.parent_map;
  rank_map=Map.add x 0 t.rank_map;
}
