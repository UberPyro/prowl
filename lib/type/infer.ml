open Type
open Hir

module Env = Map.Make(struct
  type t
  let compare = compare
end)

(* todo: generalize this beyond Cat *)
let rec expr env (dat, m0) = 
  let open Costack in
  let i0, o0 = m0#ty in
  match dat with
  | Cat [] -> unify i0 o0
  | Cat ((_, m) :: _ as ws) -> 
    let i, _ = m#ty in
    unify i0 i; 
    let rec cat = function
      | (_, m1) as h :: (_, m2) :: _ as t -> 
        let _, o1 = m1#ty in
        let i2, _ = m2#ty in
        word env h; 
        unify o1 i2; 
        cat t
      | [_, m as x] -> 
        let _, o = m#ty in
        word env x; 
        unify o0 o
      | [] -> failwith "Unreachable" in
    cat ws
  | _ -> failwith "todo"

and word _ _ = failwith "todo"

(* and word env (dat, _) = 
  match dat with
  |  *)
