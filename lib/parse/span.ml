open! Batteries

open Lexing

type t = (int * int) * (int * int) [@@deriving show]

let pos_to_lincol p = 
  p.pos_lnum, (p.pos_cnum - p.pos_bol)

let make (p1, p2) = 
  pos_to_lincol p1, pos_to_lincol p2

let lincol_cmp (l1, c1) (l2, c2) = match compare c1 c2 with
  | 0 -> compare l1 l2
  | b -> b

let lincol_max x y = 
  if lincol_cmp x y >= 0 then x else y

let lincol_min x y = 
  if lincol_cmp x y <= 0 then x else y

let join (x1, y1) (x2, y2) = 
  lincol_min x1 x2, lincol_max y1 y2
