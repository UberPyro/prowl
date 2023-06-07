open! Batteries
open Uref

type tlit = Int | String
type tcon = Quote | List

type v = v_contents uref
and v_contents = 
  | TLit of tlit
  | TCon of tcon * dc * dc
  | TMeta of int

and s = v Ull.t
and ds = s * s

and c = ds Ull.t
and dc = c * c

let rec unify_dc (x1, x2) (y1, y2) = 
  Ull.unite unify_ds occ_ds x1 y1;
  Ull.unite unify_ds occ_ds x2 y2

and unify_ds (x1, x2) (y1, y2) = 
  Ull.unite unify_v occ_v x1 y1;
  Ull.unite unify_v occ_v x2 y2

and unify_v = Uref.unite ~sel: begin curry @@ function
    | (TMeta _, v) | (v, TMeta _) -> v
    | v1, v2 when v1 = v2 -> v1
    | _ -> failwith "Type error"
  end

and occ_v i = uget %> function
  | TCon (_, (x1, y1), (x2, y2)) -> 
    List.iter (occ_c i) [x1; y1; x2; y2]
  | TMeta j when i = j -> failwith "Type error (occurs check)"
  | _ -> ()

and occ_c i = Ull.occurs occ_ds i
and occ_ds i (s1, s2) = List.iter (occ_s i) [s1; s2]
and occ_s i = Ull.occurs occ_v i
