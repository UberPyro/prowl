open! Batteries
open Printf
open Ucommon

let unify x y = 
  if x <> y then
    UnifError (Printf.sprintf "Cannot unify different names [%s] and [%s]" (uget x) (uget y))
    |> raise

let occurs _ _ = ()

type memo = unit
let memo () = ()
let refresh_memo () = ()

let generalize () _ = ()

let pretty out name = fprintf out "%s" name
