open Batteries

open Type
open Costack
open Hir
 
let rec expr (env : Env.t) (dat, m0) = 

  let i0, o0 = m0#ty in

  let sub env args (i, o) e = 
    let s = Stack.fresh () in
    let c = Costack.(fresh () |> push s) in
    let env', s' = 
      List.fold_left begin fun (e0, s0) s -> 
        let v = Var.fresh () in
        Env.set s (lit v) e0, Stack.push v s0
      end (env, s) args in
    let c' = Costack.(fresh () |> push s') in
    expr env' e; 
    unify i c'; 
    unify o c in

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

  | As (args, e) -> sub env args (i0, o0) e

  | Let (`Seq, name, args, body, e) -> 
    let m = Costack.(fresh (), fresh ()) in
    sub env args m body; 
    let env' = Env.save name m env in
    expr env' e
  
  | Let (`Rec, name, args, body, e) -> 
    let m = Costack.(fresh (), fresh ()) in
    let env' = Env.save name m env in
    sub env' args m body; 
    expr env' e

and word env (dat, _) = 
  match dat with
  | Int _ | Char _ | Id _ -> ()
  | Quote e -> expr env e
  | List es -> List.iter (expr env) es
