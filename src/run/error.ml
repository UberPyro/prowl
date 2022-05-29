open Batteries
open Lexing
open Printf

(* Todo: change so that the program outputs the error as a string,
   instead of failing, *)

let lincol_of_pos p = p.pos_lnum, (p.pos_cnum - p.pos_bol)
let span_of_loc (p1, p2) = lincol_of_pos p1, lincol_of_pos p2
let loc_err ((l1, c1), (l2, c2)) = 
  if l1 = l2 then sprintf "line %d, characters %d-%d" l1 c1 c2
  else sprintf "lines %d-%d, characters %d-%d" l1 l2 c1 c2

let print_err err span msg = 
  print_endline err;
  span |> span_of_loc |> loc_err |> print_endline;
  print_endline msg;
  failwith ""

let print_type_error = print_err "Type Error"
let print_stack_underflow = print_err "Stack Underflow"
