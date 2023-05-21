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
  | Some (h, t) -> lazy (Cons (h, append_delayed t f l2))

(* let append_delayed_uniq l1 f l2 = unique @@ append_delayed l1 f l2 *)

let rec bind f x = match get x with
  | None -> x
  | Some (h, t) -> append_delayed (f h) (bind f) t

let bind_uniq f x = unique @@ bind f x

let empty = nil
let pure x = cons x nil
let make xs = unique @@ of_list xs
let list = to_list
(* let (>>=) x f = bind_uniq f x *)
let kleisli f g x = pure x |> bind f |> bind g
(* let (>=>) = kleisli *)
let append_uniq l1 l2 = append l1 l2 |> unique
let (<|>) = append_uniq
let map_uniq f x = LazyList.map f x |> unique

let rec fix cache f s0 = 
  let res = bind f s0 |> filter @@ fun x -> 
    let is_rep = not @@ Hashtbl.mem cache x in
    if is_rep then Hashtbl.add cache x ();
    is_rep in
  if is_empty res then empty
  else append_delayed res (fix cache f) res

let plus f = pure %> fix (Hashtbl.create 16) f
let star f x = 
  let y = pure x in
  y <|> fix (Hashtbl.create 16) f y
let mark f x = 
  let y = pure x in
  y <|> bind_uniq f y
