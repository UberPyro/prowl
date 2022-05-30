open Batteries
open Lexing
open Printf

type span = Lexing.position * Lexing.position

exception ProwlError of span * string

let lincol_of_pos p = p.pos_lnum, (p.pos_cnum - p.pos_bol)
let span_of_loc (p1, p2) = lincol_of_pos p1, lincol_of_pos p2
let loc_err ((l1, c1), (l2, c2)) = 
  if l1 = l2 then sprintf "line %d, characters %d-%d" l1 c1 c2
  else sprintf "lines %d-%d, characters %d-%d" l1 l2 c1 c2

let show_err span msg =
  span |> span_of_loc |> loc_err |> fun x -> 
    sprintf "%s\n%s" x msg

let prowlfail span msg = 
  raise (ProwlError (span, msg))
