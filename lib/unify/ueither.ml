open! Batteries
open! Either
open Ucommon

type ('a, 'b) either = ('a, 'b) Either.t = 
  | Left of 'a
  | Right of 'b
  [@@deriving show]

let uleft x = uref @@ Left x
let uright x = uref @@ Right x
let uleft_delayed f () = uref @@ Left (f ())
let uright_delayed f () = uref @@ Right (f ())

type ('a, 'b) ueither = ('a, 'b) either uref
and ('a, 'b) t = ('a, 'b) ueither [@@deriving show]

let unify unify_left unify_right = 
  unite ~sel:begin curry @@ function
    | Left x, Left y -> 
      let x' = uref x in
      unify_left x' (uref y);
      Left (uget x')
    | Right x, Right y -> 
      let x' = uref x in
      unify_right x' (uref y);
      Right (uget x')
    | _ -> 
      UnifError "Cannot unify differently sorted terms"
      |> raise
  end

let occurs occurs_left occurs_right i = uget %> function
  | Left x -> occurs_left i x
  | Right x -> occurs_right i x
