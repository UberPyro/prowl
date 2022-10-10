open Batteries

open Type
open Hir

module Env = Map.Make(struct
  type t = string
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
  | As (ss, e) -> 
    let s = Stack.fresh () in
    let c = Costack.(fresh () |> push s) in
    let env', s' = 
      List.fold_left begin fun (e0, s0) s -> 
        let v = Var.fresh () in
        Env.add s (lit v) e0, Stack.push v s0
      end (Env.empty, s) ss in
    let c' = Costack.(fresh () |> push s') in
    expr env' e; 
    unify i0 c'; 
    unify o0 c
  | _ -> failwith "todo"

and word _ _ = failwith "todo"

(* and word env (dat, _) = 
  match dat with
  |  *)
