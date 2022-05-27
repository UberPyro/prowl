open Batteries
open LazyList.Infix

let (<&>) x f = LazyList.map f x
let (>>=) x f = x <&> f |> LazyList.concat
let (>=>) f g x = f x >>= g
let pure a = LazyList.(cons a nil)
let (<|>) f g x = f x ^@^ g x
let ( *> ) x c y = x y >>= fun _ -> c y
let empty _ = LazyList.nil
