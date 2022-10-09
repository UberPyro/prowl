open Type

module Env = Map.Make(struct
  type t
  let compare = compare
end)

let rec expr env (dat, m0) = 
  let open Costack in
  let i0, o0 = m0#ty in
  match dat with
  | [] -> unify i0 o0
  | (_, m) :: _ -> 
    let i, _ = m#ty in
    unify i0 i; 
    let rec cat = function
      | (_, m1) as h :: (_, m2) :: _ as t -> 
        let _, o1 = m1#ty in
        let i2, _ = m2#ty in
        word env h; 
        unify o1 i2; 
        cat t
      | [(_, m) as x] -> 
        let _, o = m#ty in
        word env x; 
        unify o0 o
      | [] -> failwith "Unreachable" in
    cat dat

and word _ _ = failwith "todo"

(* and word env (dat, _) = 
  match dat with
  |  *)
