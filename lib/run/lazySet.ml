open! Batteries

module LazyList = struct
  include LazyList
  module PP = struct
    type 'a t = 'a list [@@deriving show]
  end
  let pp f fmt = to_list %> PP.pp f fmt
end

open LazyList

type 'a t = 'a LazyList.t [@@deriving show]

let rec append_delayed l1 f l2 = match get l1 with
  | None -> f l2
  | Some (h, t) -> unique @@ lazy (Cons (h, append_delayed t f l2))

let rec bind f x = match get x with
  | None -> x
  | Some (h, t) -> append_delayed (f h) (bind f) t

let empty = nil
let pure x = cons x nil
let make xs = unique @@ of_list xs
let list = to_list
let (>>=) x f = bind f x
let (>=>) f g x = pure x >>= f >>= g
let (<|>) l1 l2 = append l1 l2 |> unique

let rec fix cache f s0 = 
  let res = s0 >>= f |> filter @@ fun x -> 
    if Hashtbl.mem cache x then false
    else (Hashtbl.add cache x (); true) in
  if is_empty res then empty
  else append_delayed res (fix cache f) res

let plus f = pure %> fix (Hashtbl.create 16) f %> unique
let star f x = 
  let y = pure x in
  y <|> fix (Hashtbl.create 16) f y

let ( ~* ) = star
let ( ~+ ) = plus
