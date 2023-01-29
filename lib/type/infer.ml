open! Batteries

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
    let io_term = Tuple3.third e in
    connect io_pat io_term;
    connect_in io0 io_pat;
    connect_out io0 io_term

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
