open! Batteries
open Uref

open Syntax

exception EmptyStack

let pp_uref fmt x y = fmt x (uget y)

let c = ref (-1)
let (!!)() = 
  incr c;
  !c

module LazyList = struct
  include LazyList
  module PP = struct
    type 'a t = 'a list [@@deriving show]
  end
  let pp f fmt = to_list %> PP.pp f fmt

  let[@tail_cons_mod] rec merge_inter l1 l2 = 
    match get l1, get l2 with
    | Some (h1, t1), Some (h2, t2) -> 
      if h1 = h2 then lazy (Cons (h1, merge_inter t1 t2))
      else if h1 < h2 then merge_inter t1 l2
      else merge_inter l1 t2
    | _ -> nil
  
  let[@tail_cons_mod] rec merge_uniq l1 l2 = 
    match LazyList.(get l1, get l2) with
    | Some (h1, t1), Some (h2, t2) -> lazy begin
      if h1 = h2 then LazyList.Cons (h1, merge_uniq t1 t2)
      else if h1 < h2 then LazyList.Cons (h1, merge_uniq t1 l2)
      else LazyList.Cons (h2, merge_uniq l1 t2)
    end
    | _, None -> l1
    | None, _ -> l2

  let pure x = cons x nil

end

let ( *> ) = LazyList.merge_inter
let (<|>) = LazyList.merge_uniq
let pure = LazyList.pure

module Dict = struct
  include Map.Make(String)
  module D = struct
    type 'a t = (string * 'a) list [@@deriving show]
  end
  let pp h fmt = bindings %> D.pp h fmt
end

type _value = [
  | Prim.lit
  | `closure of Mir.expr * context
  | `closedList of (Mir.expr * context) list
  | `quotedValue of value
] [@@deriving show]

and _value_poly = 
  | Bound of _value LazyList.t  (* maintain order *)
  | Free of int
  [@@deriving show]

and value = _value_poly uref [@@deriving show]

and context = Mir.expr Dict.t

type stack = value list

type costack = 
  | Real of stack
  | Fake of costack

let unify = unite ~sel:begin fun x0 y0 -> match x0, y0 with
  | Bound _ as b, Free _ | Free _, b -> b
  | Bound x, Bound y -> Bound (x *> y)
end

let comap f = function
  | Real s -> Real (f s)
  | c -> c

let cobind f = function
  | Real s -> f s
  | c -> c

let pop = function
  | h :: t -> t, h
  | [] -> raise @@ EmptyStack

let pop2 = function
  | h1 :: h2 :: t -> h1, h2, t
  | _ -> raise @@ EmptyStack

let push t h = h :: t
let push2 t h2 h1 = h1 :: h2 :: t
