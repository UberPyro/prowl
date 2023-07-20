open! Batteries
open Ucommon
open Printf

let pp_uref fmt x y = fmt x (uget y)

type 'a ust = 'a ust_ uref
and 'a ust_ = 
  | USyntax of 'a * 'a ust list
  | UVar of int
and 'a t = 'a ust [@@deriving show]

let usyn n us = uref @@ USyntax (n, us)
let uvar () = uref @@ UVar (unique ())

let rec unify unify_elem occurs_elem r = 
  let sel x y = match x, y with
    | UVar _, UVar _ -> x
    | UVar i, USyntax (_, us) -> 
      List.iter (occurs occurs_elem i) us;
      y
    | USyntax (_, us), UVar i -> 
      List.iter (occurs occurs_elem i) us;
      x
    | USyntax (elem1, args1), USyntax (elem2, args2) -> 
      unify_elem elem1 elem2; 
      try List.iter2 (unify unify_elem occurs_elem) args1 args2; x
      with Invalid_argument _ -> 
        UnifError (
          sprintf "Cannot unify terms of arity [%d] and [%d]"
          (List.length args1) (List.length args2))
        |> raise in
  Uref.unite ~sel r

and occurs occurs_elem i = uget %> function
    | UVar j when i = j -> 
      UnifError "Cannot unify a variable with syntax that contains it"
      |> raise
    | UVar _ -> ()
    | USyntax (n, us) -> 
      occurs_elem i n;
      List.iter (occurs occurs_elem i) us
