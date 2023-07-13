open! Batteries
open Ull
open Uref

type boolean = 
  | T | F
  | And of boolean * boolean
  | Or of boolean * boolean
  | BVar

and balg = boolean array uref [@@deriving show]

let bfresh () = uref [|BVar|]
