open! Batteries
open Lexing

module PP = struct
  type t = (int * int) * (int * int) [@@deriving show]
end

type t = Lexing.position * Lexing.position

let lincol_of_pos p = p.pos_lnum, (p.pos_cnum - p.pos_bol)
let span_of_loc (p1, p2) = lincol_of_pos p1, lincol_of_pos p2

let pp fmt = span_of_loc %> PP.pp fmt
let show = span_of_loc %> PP.show

let dummy = Lexing.(dummy_pos, dummy_pos)
