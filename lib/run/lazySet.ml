open! Batteries

open Either

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
let (<|>) l1 l2 = append_delayed l1 Fun.id l2

let interdiff s1_init s2_init = 

  let rec s1_populated s1 s2 cache = match get s2 with
    | Some (h2, t2) -> 
      if Set.mem h2 cache
      then lazy (Cons (Left h2, s1_populated s1 t2 (Set.remove h2 cache)))
      else s2_selected s1 t2 cache h2
    | None -> nil
  
  and s2_selected s1 s2 cache h2 = match get s1 with
    | Some (h1, t1) -> 
      if h1 = h2 then lazy (Cons (Left h2, s1_populated t1 s2 (Set.remove h2 cache)))
      else s2_selected t1 s2 (Set.add h1 cache) h2
    | None -> lazy (Cons (Right h2, (s1_forced s2 cache)))
  
  and s1_forced s2 cache = 
    map (fun x -> if Set.mem x cache then Left x else Right x) s2 in
  
  s1_populated s1_init s2_init Set.empty |> enum |> Enum.switch is_left
  |> Tuple2.mapn (Enum.map Fun.(Either.fold ~left:id ~right:id) %> of_enum)

let star f x = 
  let rec go s1 = 
    let s2 = s1 >>= f in
    let s3, s4 = interdiff s1 s2 in
    append_delayed s3 go s4 in
  go @@ pure x

let ( ~* ) = star
