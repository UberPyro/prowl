open! Batteries

open Type
open Ast

(* let rec check_program env prog = List.map (check_component env) prog

and check_component env (comp, _) = match comp with
  | Def (s, e) -> infer env e *)

(* assume literals are created correctly *)
let infer env' = fold_expr env' {
  id = (fun _ _ _ -> ());
  var = (fun _ _ _ -> ());
  seq = (fun _ _ _ -> ());
  coseq = (fun _ _ _ -> ());
  quote = (fun env _ f -> f env);
  list = (fun env _ ls -> List.iter (fun f -> f env) ls);

  int = (fun _ _ _ -> ());
  float = (fun _ _ _ -> ());
  char = (fun _ _ _ -> ());
  string = (fun _ _ _ -> ());

  tag = (fun _ _ _ -> ());

  cat = failwith "todo";
  binop = failwith "todo";
  unop = failwith "todo";
  left_sect = failwith "todo";
  right_sect = failwith "todo";
  sect = failwith "todo";

  let_ = failwith "todo";
  arrow = failwith "todo";
}
