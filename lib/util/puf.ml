(* http://fhur.github.io/data-structure/clojure/2016/04/01/implementing-immutable-union-find-with-clojure.html *)
open! Batteries

module Make(Elem : Map.OrderedType) = struct

  module ElemMap = Map.Make(Elem)

  type t = {
    parent_map : Elem.t ElemMap.t;
    rank_map : int ElemMap.t;
  }

  open ElemMap

  let parent x t = find x t.parent_map
  let rank x t = find x t.rank_map

  let rec find_root x t = 
    let p = parent x t in
    if Elem.compare p x = 0 then x
    else find_root p t
  
  let unite x y t = 
    let px = find_root x t in
    let py = find_root y t in
    if Elem.compare px py = 0 then t
    else match[@warning "-8"] Stdlib.compare (rank x t) (rank y t) with
    | 1 -> {t with parent_map=add py px t.parent_map}
    | -1 -> {t with parent_map=add px py t.parent_map}
    | 0 -> {
      parent_map = add px py t.parent_map;
      rank_map = update_stdlib py 
        (fun[@warning "-8"] (Some i) -> Some (i + 1)) t.rank_map;
    }

end
