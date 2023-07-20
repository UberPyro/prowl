open! Batteries
open Ucommon

let unify x y = 
  if x <> y then
    UnifError (Printf.sprintf "Cannot unify different names [%s] and [%s]" (uget x) (uget y))
    |> raise

let occurs _ _ = ()
