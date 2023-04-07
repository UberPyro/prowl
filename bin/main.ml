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
(* let plus x = `plus x, sp *)
let mul = `mul, sp
(* let zap = `zap, sp *)
let dup = `dup, sp
let ex s e = `ex (s, e), sp
let uvar x = `uvar x, sp
let def bs e = `bind_var (bs, e), sp
let id i = `id i, sp
let pick x = `pick x, sp
let pred = `pred, sp
let eq = `eq, sp
let dag e = `dag e, sp

let () = 
  LazyList.iter (Eval_mir.show_costack %> print_endline) @@ 
    Eval_mir.expr (Eval_mir.init ()) (def [
      "fac", ex ["N"] (jux [
        dag (uvar "N"); 
        uvar "N"; int 0; eq; pick [
          int 1;
          uvar "N"; dup; pred; id "fac"; mul
        ]
      ])
    ] (jux [int 7; id "fac"])) (Real [])
