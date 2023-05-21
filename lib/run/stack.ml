(* linked lists that retain total height *)
open! Batteries

type 'a t = 'a list * int [@@deriving show]

let push x (xs, i) = x :: xs, i + 1
let push2 x1 x2 (xs, i) = x1 :: x2 :: xs, i + 2
let pop (xs, i) = match[@warning "-8"] xs with
  | h :: t -> h, (t, i-1)
let pop2 (xs, i) = match[@warning "-8"] xs with
  | h1 :: h2 :: t -> h1, h2, (t, i - 2)

let of_list xs = xs, List.length xs

let len (_, i) = i
