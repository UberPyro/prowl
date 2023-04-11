[@@@warning "-32"]
(* let () = 
  print_newline ();
  print_endline "\"A journey of a thousand miles begins with a single step.\"";
  print_endline "(todo: put something less insightful here)" *)

open! Batteries
open! Prowl

open! Syntax
open! Run

let sp = Metadata.Span.dummy

let () = print_newline ()

let jux x = `jux x, sp
let int x = `int x, sp
let star x = `star x, sp
(* let plus x = `plus x, sp *)
let mul = `mul, sp
let zap = `zap, sp
let dup = `dup, sp
let ex s e = `ex (s, e), sp
let uvar x = `uvar x, sp
let def bs e = `bind_var (bs, e), sp
let id i = `id i, sp
let pick x = `pick x, sp
let ponder x = `ponder x, sp
let pred = `pred, sp
let eq = `eq, sp
let dag e = `dag e, sp
let succ = `succ, sp
let debug = `debug, sp
let exch = `exch, sp
let gen = `gen, sp
let fab = `fab, sp
let elim = `elim, sp
let cmp = `cmp, sp
let fork es = `fork es, sp
let add = `add, sp
let mk = `mk, sp
let list es = `list es, sp
let par es = `par es, sp
let swap = `swap, sp
let quote e = `quote e, sp
let dip = `dip, sp

let () = 
  Print_eval_mir.print @@ Eval_mir.expr (Eval_mir.init ()) (
    def [
      "append", jux [
        dag mk; pick [
          par [
            mk;
            jux [id "append"; gen];
          ];
          jux [];
        ]
      ]
    ] (jux [list [int 2; int 1]; dag @@ id "append"])
  ) (Real (Sys.argv |> Array.to_list |> List.tl |> List.map (fun x -> 
    Uref.uref @@ `int (String.to_int x))))

(* let () = 
  LazyList.iter (Eval_mir.show_costack %> print_endline) @@ 
    Eval_mir.expr (Eval_mir.init ()) (
      def [
        "fib", jux [
          dup; int 1; cmp; pred;
          fork [
            id "fib";
            jux [pred; id "fib"];
          ];
          add; elim; elim
        ]
      ] (id "fib")
    ) (Real (Sys.argv |> Array.to_list |> List.tl |> List.map (fun x -> 
      Uref.uref @@ `int (String.to_int x)))) *)

(* let () = 
  LazyList.iter (Eval_mir.show_costack %> print_endline) @@ 
    Eval_mir.expr (Eval_mir.init ()) (
      def [
        "fib", ex ["N"] (jux [
          dag (uvar "N");
          uvar "N"; int 1; 
          debug;
          cmp; 
          debug;
          pick [
            jux [
              uvar "N"; pred;
              fork [
                id "fib";
                jux [pred; id "fib"];
              ];
              add;
            ];
            int 1;
            int 0;
          ]
        ])
      ] (id "fib")
    ) (Real (Sys.argv |> Array.to_list |> List.tl |> List.map (fun x -> 
      Uref.uref @@ `int (String.to_int x)))) *)

(* let () = 
  LazyList.iter (Eval_mir.show_costack %> print_endline) @@ 
    Eval_mir.expr (Eval_mir.init ()) (def [
      "fac", ex ["N"] (jux [
        debug;
        dag (uvar "N"); 
        uvar "N"; int 0; eq; pick [
          int 1;
          jux [uvar "N"; dup; pred; id "fac"; mul]
        ]
      ])
    ] (jux [int 7; id "fac"])) (Real []) *)

(* let () = 
  LazyList.iter (Eval_mir.show_costack %> print_endline) @@ 
    Eval_mir.expr (Eval_mir.init ()) (def [
      "fac", ex ["N"] (jux [
        dag (uvar "N"); 
        uvar "N"; int 0; eq; pick [
          int 1;
          jux [uvar "N"; dup; pred; id "fac"; mul]
        ]
      ])
    ] (jux [int 7; id "fac"])) (Real []) *)
