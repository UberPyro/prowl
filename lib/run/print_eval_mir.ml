open! Batteries
open Eval_mir
open Uref
open LazyList
open Printf

let rec printn n s = take n s |> print

and print s = 
  print_set s;
  print_newline ()

and print_set s = match get s with
  | None -> ()
  | Some (h, t) -> 
    print_costack h;
    iter begin fun c -> 
      print_newline ();
      print_costack c
    end t

and print_costack = function
  | Real s -> print_stack s
  | Fake c -> 
    printf "<Fake> ";
    print_costack c

and print_stack = function
  | [] -> ()
  | [v] -> print_value v
  | h :: t -> 
    print_stack t;
    printf " ";
    print_value h

and print_value v = match uget v with
  | `int i -> printf "%d" i
  | `str s -> printf "%s" s
  | #callable -> 
    printf "[";
    call v (Real []) |> print_set;
    printf "]"
  | `closedList cs -> 
    printf "{";
    begin match cs with
      | [] -> ()
      | #callable as h :: t -> 
        print_set (call (uref h) (Real []));
        t |> List.iter begin fun (#callable as c) -> 
          printf ", ";
          call (uref c) (Real []) |> print_set
        end
    end;
    printf "}"
  | `free -> printf "<free>"
  | `empty -> printf "<empty>"
