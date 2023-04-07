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

let jux x = `jux x, sp
let int x = `int x, sp
(* let star x = `star x, sp *)
let plus x = `plus x, sp
let zap = `zap, sp

let () = 
  LazyList.iter (Eval_mir.show_costack %> print_endline) @@ 
    Eval_mir.expr (Eval_mir.init ()) (jux [
      int 0; plus (jux [
        zap;
        int 1;
      ])
    ]) (Real [])
