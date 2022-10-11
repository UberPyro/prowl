open Batteries

open Type
open Hir

let handle f locs = try f with
  | Error.Unification (u, v) -> 
    raise @@ Ast.Prowl_error begin
      Printf.sprintf
        "Type Error: Cannot unify type [%s] with type [%s]."
        (Var.show__t u)
        (Var.show__t v), 
      locs
    end

let unify_exn = Costack.unify
let unify = handle Costack.unify

let rec expr (env : Env.t) (dat, m0) = 

  let i0, o0 = m0#ty in

  let sub_exn env args (i, o) e = 
    let s = Stack.fresh () in
    let c = Costack.(fresh () |> push s) in
    let env', s' = 
      List.fold_left begin fun (e0, s0) s -> 
        let v = Var.fresh () in
        Env.set s (lit v) e0, Stack.push v s0
      end (env, s) args in
    let c' = Costack.(fresh () |> push s') in
    expr env' e; 
    unify_exn i c'; 
    unify_exn o c in
  
  let sub env args m e = 
    (handle sub_exn [(snd e)#loc]) env args m e in

  match dat with
  | Cat [] -> unify [m0#loc] i0 o0
  | Cat ((_, m) :: _ as ws) -> 
    let i, _ = m#ty in
    unify [m#loc] i0 i; 
    let rec cat = function
      | (_, m1) as h :: (_, m2) :: _ as t -> 
        let _, o1 = m1#ty in
        let i2, _ = m2#ty in
        word env h; 
        unify [m1#loc; m2#loc] o1 i2; 
        cat t
      | [_, m as x] -> 
        let _, o = m#ty in
        word env x; 
        unify [m#loc] o0 o
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
    let env' = Env.set name m env in
    sub env' args m body; 
    expr (Env.promote name env') e

and word env (dat, _) = 
  match dat with
  | Int _ | Char _ | Id _ -> ()
  | Quote e | Group e -> expr env e
  | List es -> List.iter (expr env) es
