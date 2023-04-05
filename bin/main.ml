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

let jx x = `jux (List.map (fun x -> x, sp) x)
(* let dg x = `dag (x, sp)
let ds x y = `dis ((x, sp), (y, sp)) *)
(* let pk x = `pick (List.map (fun x -> x, sp) x) *)

let () = LazyList.iter (Eval_mir.show_costack %> print_endline) @@ 
  Eval_mir.expr Eval_mir.init (jx [
    `int 7; `int 1; 
    `star (jx [
      `quote (jx [
        `dup; `int 0; `eq; `exch; `pred; `dup
      ], sp);
      `dip; `mul; `elim;
    ], sp);
    `swap; `zap;
  ], sp) (Real [])
