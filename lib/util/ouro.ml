open! Batteries
open Tuple3
open Lazy

type color = R | B
and ('a, 'b) t = 
  | E
  | T of color * ('a, 'b) t * ('a * 'b * ('a, 'b) t Lazy.t) * ('a, 'b) t
  [@@deriving show]

let rec fix f x = f (fix f) x

let empty = E

let rec find_rec_opt k = function
  | E -> None
  | T (_, a, y, b) -> 
    match[@warning "-8"] Stdlib.compare k (first y) with
    | -1 -> find_rec_opt k a
    | 0 -> Some (second y, third y)
    | 1 -> find_rec_opt k b

let find_rec k s = match find_rec_opt k s with
  | Some v -> v
  | None -> raise (Invalid_argument "find")

let balance = function
  | T (B, T (R, T (R, a, x, b), y, c), z, d)
  | T (B, T (R, a, x, T (R, b, y, c)), z, d)
  | T (B, a, x, T (R, T (R, b, y, c), z, d)) 
  | T (B, a, x, T (R, b, y, T (R, c, z, d))) -> 
    T (R, T (B, a, x, b), y, T (B, c, z, d))
  | t -> t

let[@warning "-8"] blacken (T (_, a, y, b)) = T (B, a, y, b)

let insert k v s = 
  let rec ins = function
    | E -> T (R, E, (k, v, s'), E)
    | T (color, a, y, b) as t -> 
      match[@warning "-8"] Stdlib.compare k (first y) with
      | -1 -> balance (T (color, ins a, y, b))
      | 0 -> t
      | 1 -> balance (T (color, a, y, ins b))
  and s' = lazy (blacken (ins s)) in
  force s'

let insert_many kvs s = 
  let rec ins k v = function
    | E -> T (R, E, (k, v, s'), E)
    | T (color, a, y, b) as t -> 
      match[@warning "-8"] Stdlib.compare k (first y) with
      | -1 -> balance (T (color, ins k v a, y, b))
      | 0 -> t
      | 1 -> balance (T (color, a, y, ins k v b))
  and s' = lazy (List.fold_right (fun (k, v) s -> 
    blacken (ins k v s)) kvs s) in
  force s'

let rec to_list = function
  | E -> []
  | T (_, l, (k, v, _), r) -> to_list l @ (k, v) :: to_list r

type ('a, 'b) collected = ('a * 'b) list [@@deriving show]
let pp f g h = to_list %> pp_collected f g h

let rec vmap f t = match t with
    | E -> E
    | T (c, a, (k, v, lazy r), b) -> 
      T (c, vmap f a, (k, f v, lazy (vmap f r)), vmap f b)
