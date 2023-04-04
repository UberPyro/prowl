(* let () = 
  print_newline ();
  print_endline "\"A journey of a thousand miles begins with a single step.\"";
  print_endline "(todo: put something less insightful here)" *)

open! Batteries
open! Prowl

open! Syntax
open! Run

let sp = 0, 0

let () = print_newline ()

let () = LazyList.iter (Eval_mir.show_costack %> print_endline) @@ 
  Eval_mir.expr Eval_mir.Dict.empty (`jux [
    `str "Hello, World!", sp
  ], sp) (Real [])
