(* http://fhur.github.io/data-structure/clojure/2016/04/01/implementing-immutable-union-find-with-clojure.html *)
open! Batteries
open Map

type 'a t = {
  parent_map : ('a, 'a) Map.t;
  rank_map : ('a, int) Map.t;
}

let parent x t = find x t.parent_map
let rank x t = find x t.rank_map

let rec find_root x t = 
  let p = parent x t in
  if Stdlib.compare p x = 0 then x
  else find_root p t

let unite x y t = 
  let px = find_root x t in
  let py = find_root y t in
  if Stdlib.compare px py = 0 then t
  else match[@warning "-8"] Stdlib.compare (rank x t) (rank y t) with
  | 1 -> {t with parent_map=add py px t.parent_map}
  | -1 -> {t with parent_map=add px py t.parent_map}
  | 0 -> {
    parent_map = add px py t.parent_map;
    rank_map = update_stdlib py 
      (fun[@warning "-8"] (Some i) -> Some (i + 1)) t.rank_map;
  }

let mk xs = {
  parent_map=xs |> List.enum |> Enum.map (fun x -> x, x) |> of_enum;
  rank_map=xs |> List.enum |> Enum.map (fun x -> x, 0) |> of_enum;
}
