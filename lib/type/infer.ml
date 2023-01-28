open! Batteries

open Type
open Ast

(* let rec check_program env prog = List.map (check_component env) prog

and check_component env (comp, _) = match comp with
  | Def (s, e) -> infer env e *)

let rec fix f n = f (fix f) n
let pass _ (_, i, o) _ = i, o

(* assume literals are created correctly *)
let infer env' = fold_expr env' {
  id = pass;
  var = pass;
  seq = pass;
  coseq = pass;
  quote = (fun env (_, i, o) f -> f env |> ignore; i, o);
  list = (fun env (_, i, o) ls -> 
    List.map (fun f -> f env) ls |> ignore; i, o);

  int = pass;
  float = pass;
  char = pass;
  string = pass;

  tag = pass;

  cat = begin fun env (_, i0, o0) fs -> 
    begin match List.map (fun f -> f env) fs with
    | [] -> unify_costack i0 o0
    | (i, _) :: _ as ws -> 
      unify_costack i0 i; 
      ws |> fix begin fun cat -> function
        | (_, o) :: (i, _) :: t -> 
          unify_costack i o; 
          cat t
        | [_, o] -> unify_costack o0 o
        | [] -> failwith "Unreacheable"
      end
    end; 
    i0, o0
  end;



  binop = failwith "todo";
  unop = failwith "todo";
  left_sect = failwith "todo";
  right_sect = failwith "todo";
  sect = failwith "todo";

  let_ = failwith "todo";
  arrow = failwith "todo";
}
