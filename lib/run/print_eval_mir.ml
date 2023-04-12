open! Batteries
open Eval_mir
open Uref
open LazyList
open Printf

let rec printn fmt n s = take n s |> print fmt

and print fmt s = 
  print_set fmt s;
  fprintf fmt "\n"

and print_set fmt s = match get s with
  | None -> ()
  | Some (h, t) -> 
    print_costack fmt h;
    iter begin fun c -> 
      fprintf fmt "\n";
      print_costack fmt c
    end t

and print_costack fmt = function
  | Real s -> print_stack fmt s
  | Fake c -> 
    fprintf fmt "<Fake> ";
    print_costack fmt c

and print_stack fmt = function
  | [] -> ()
  | [v] -> print_value fmt v
  | h :: t -> 
    print_stack fmt t;
    fprintf fmt " ";
    print_value fmt h

and print_value fmt v = match uget v with
  | `int i -> fprintf fmt "%d" i
  | `str s -> fprintf fmt "%s" s
  | #callable -> 
    fprintf fmt "[";
    call v (Real []) |> print_set fmt;
    fprintf fmt "]"
  | `closedList cs -> 
    fprintf fmt "{";
    begin match cs with
      | [] -> ()
      | #callable as h :: t -> 
        t |> List.rev |> List.iter begin fun (#callable as c) -> 
          call (uref c) (Real []) |> print_set fmt;
          fprintf fmt ", "
        end;
        print_set fmt (call (uref h) (Real []))
    end;
    fprintf fmt "}"
  | `free -> fprintf fmt "<free>"
  | `empty -> fprintf fmt "<empty>"
