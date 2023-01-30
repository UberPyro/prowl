open! Batteries
open Tuple3

open Type
open Ast

let rec fix f n = f (fix f) n

let rec expr env (e_, _, io0) = match e_ with
  | Cat [] -> connect_self io0
  | Cat ((_, _, io) :: _ as es) -> 
    List.iter (expr env) es; 
    connect_in io0 io;
    es |> fix begin fun cat -> function
      | (_, _, io1) :: (_, _, io2) :: _ as t -> 
        connect io1 io2;
        cat t
      | [_, _, io] -> connect_out io0 io
      | [] -> failwith "Unreachable"
    end

  | Lam (p, e) -> 
    expr env e;
    let io_pat = fst @@ pat env p in
    let io_term = third e in
    connect io_pat io_term;
    connect_in io0 io_pat;
    connect_out io0 io_term
  
  | Let (bindings, e) -> 
    let env' = List.fold_left (fun env' (b, e) -> 
      Env.set b (third e) env') env bindings in
    List.iter (snd %> expr env) bindings;
    expr (List.fold_left (fun env_ (b, _) -> 
      Env.promote b env_) env' bindings) e

  | Var s -> Env.unite s (get_right io0) env
  | Seq i -> Env.unite_stack i (get_right_stack io0) env
  | Coseq i -> Env.unite_costack i (snd io0) env
  
  | Id _ | Int _ | Float _ | Char _ | String _ | Tag _ | Sect _ -> ()
  | Quote e -> expr env e
  | List es -> List.iter (expr env) es

  (* Assume binops are generated w/ quotes already concrete *)
  | Binop (e1, eo, e2) -> 
    expr env e1;
    expr env e2;
    expr env eo;
    let io1, io2, io3 = dequote @@ third eo in
    connect_parallel io1 @@ third e1;
    connect_parallel io2 @@ third e2;
    connect_parallel io3 io0
  
  | Unop (e, eo) -> 
    expr env e;
    expr env eo;
    let io1, io2 = dequote_unary @@ third eo in
    connect_parallel io1 @@ third e;
    connect_parallel io2 io0
  
  (* | LeftSect (eo, e) ->  *)

  | Meet es | Join es -> 
    List.iter (expr env) es;
    fix begin fun f -> function
      | e1 :: e2 :: t -> 
        connect_parallel (third e1) (third e2);
        f t
      | [_] | [] -> ()
    end es
  
  | Conj (e1, e2) -> connect_parallel (third e1) (third e2)


  | _ -> failwith "todo"

and pat env_ (p_, _) = 
  (env_, (p_, unconnected ())) |> fix begin fun f (env, (p, io)) -> 
    let push_mono nom = push_left io nom, env in
    (* let push_poly1 nom p = 
      let io', env' = pat env p in
      push_left io (nom io'), env' in *)
    
    match p with
    | PId s -> 
      let v = fresh_var () in
      push_left io v, Env.set s (lit v) env
    
    | PInt _ -> push_mono nom_int
    | PFloat _ -> push_mono nom_float
    | PChar _ -> push_mono nom_char
    | PString _ -> push_mono nom_string
    | PQuote s -> 
      let io' = unconnected () in
      push_left io (nom_quote io'), Env.set s (io') env
    
    | PCat ps -> List.fold_right (fun (p, _) (io', env') -> 
      f (env', (p, io'))) ps (io, env)

    | PConj ((p1, _), (p2, _)) -> 
      let io1, env1 = f (env, (p1, io)) in
      let io2, env2 = f (env1, (p2, io)) in
      connect_parallel io1 io2;
      io1, env2
  end
