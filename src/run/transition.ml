open Batteries
open LazyList.Infix

let (<&>) x f = LazyList.map f x
let (>>=) x f = x <&> f |> LazyList.concat
let (>=>) f g x = f x >>= g
let pure a = LazyList.(cons a nil)
let (<|>) f g x = f x ^@^ g x
let ( *> ) x c y = x y >>= fun _ -> c y
let empty _ = LazyList.nil

let lit st v = pure (push v st)

open Ast

let encode_lst loc = List.fold_left begin fun a ex -> 
  Right (Pair ((a, loc), ex), loc)
end (Left (Unit, loc))

let encode_plst loc = List.fold_left begin fun a ex -> 
  PRight (PPair ((a, loc), ex), loc)
end (PLeft (PUnit, loc))
