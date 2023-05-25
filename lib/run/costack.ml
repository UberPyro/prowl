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
    if j > 0 then Either.Left (x, j - 1, i - 1)
    else Either.Right (x, i - 1)
  else raise (Invalid_argument "Costack.shift")
let origin x = x, 0, 1

let height (_, _, i) = i

let assume c = match[@warning "-8"] shift c with
  | Either.Right (x, cd) -> x, cd
let designate x cd = x, 0, cd

let nk n (value, degree, height) = 
  assert (height - n >= 0);
  if degree - n >= 0 then Some (value, degree - n, height - n)
  else None

let deepen n (value, degree, height) = (value, degree + n, height + n)
