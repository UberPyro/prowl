open! Batteries
open Uref

open Syntax

exception EmptyStack
exception Polycall

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

  let flat_uniq x = 
    LazyList.fold_left (fun acc e -> merge_uniq acc (sort e)) LazyList.nil x
  
  let bind_uniq f x = LazyList.map f x |> flat_uniq

end

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

let ( *> ) = curry @@ function
  | Bound _ as b, Free _ | Free _, b -> b
  | Bound b1, Bound b2 -> Bound (LazyList.merge_inter b1 b2)


let (<|>) = curry @@ function
  | Free _ as f, Bound _ | _, (Free _ as f) -> f
  | Bound b1, Bound b2 -> Bound (LazyList.merge_uniq b1 b2)

let empty = Bound (LazyList.nil)
let pure v = Bound (LazyList.pure v)

let flat_val x = LazyList.fold_left (<|>) empty x

(* note: it has to be the job of the callee to decide
   what to do on the polymorphic case *)

let unify = unite ~sel:( *> )

let comap f = function
  | Real s -> Real (f s)
  | c -> c

let (let+) x f = comap f x

let cobind f = function
  | Real s -> f s
  | c -> c

let (let*) x f = cobind f x

let pop = function
  | h :: t -> t, h
  | [] -> raise @@ EmptyStack

let pop2 = function
  | h1 :: h2 :: t -> t, h2, h1
  | _ -> raise @@ EmptyStack

let push t h = h :: t
let push2 t h2 h1 = h1 :: h2 :: t

let lit x = uref @@ pure x

let rec expr ctx ((e_, _) : Mir.expr) i = match e_ with
  | `gen -> begin match i with
    | Real _ -> i
    | Fake _ -> Fake i
  end
  | `fab -> Fake i
  | `elim -> begin match i with
    | Real _ -> i
    | Fake j -> j
  end
  | `exch -> begin match i with
    | Real _ -> Fake i
    | Fake j -> j
  end
  | `swap -> comap (pop2 %> fun (s, v2, v1) -> push2 s v1 v2) i
  | `unit -> comap (pop %> fun (s, v) -> push s (lit @@ `quotedValue v)) i
  (* | `call -> cobind (pop %> fun (s, v) -> match uget v with
    | Free _ -> raise Polycall
    | Bound b -> LazyList.map (function _v -> 
      | `closure (e', ctx') -> expr ctx' e' (Real s)
    ) b
  ) i *)



  | _ -> failwith "todo"
