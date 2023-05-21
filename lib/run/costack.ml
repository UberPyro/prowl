(* Length-tracking dualized stacks *)
open! Batteries

(* theoretical representation (unsized) *)
(* type 'a _t = 
  | Real of 'a
  | Fake of 'a _t *)

(* efficient representation *)
(* real value * degree removed * height *)
type 'a t = 'a * int * int [@@deriving show]

let real x (_, _, i) = x, 0, i
let fake (x, j, i) = x, j + 1, i + 1
let shift (x, j, i) = 
  if i > 1 then
    if j > 0 then None, (x, j - 1, i - 1)
    else Some x, (x, j, i - 1)
  else raise (Invalid_argument "Costack.shift")
let origin x = x, 0, 1

let height (_, _, i) = i
