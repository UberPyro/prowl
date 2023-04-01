open! Batteries
open Uref

open Syntax

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

  let rec merge_uniq l1 l2 = match get l1, get l2 with
    | Some (h1, t1), Some (h2, t2) -> lazy begin
      if h1 = h2 then Cons (h1, merge_uniq t1 t2)
      else if h1 < h2 then Cons (h1, merge_uniq t1 l2)
      else Cons (h2, merge_uniq l1 t2)
    end
    | _, None -> l1
    | None, _ -> l2
  
  let[@tail_cons_mod] rec map_pairs f l = match l with
    | [] | [_] -> l
    | h1 :: h2 :: t -> f h1 h2 :: map_pairs f t
  
  let rec combine_uniq l = match l with
    | [] -> nil
    | h :: t -> 
      if List.is_empty t then h
      else combine_uniq (map_pairs merge_uniq l)
  
  let rec rev_ap ?(onto=nil) = function
    | h :: t -> rev_ap ~onto:(lazy (Cons (h, onto))) t
    | [] -> onto
      
  let build x = 
    let[@tail_cons_mod] rec go temp l = match get l with
      | None -> [rev_ap temp]
      | Some (hl, tl) -> match temp with
        | [] -> go (hl :: temp) tl
        | h_temp :: _ -> 
          if h_temp < hl then go (hl :: temp) tl
          else rev_ap temp :: go [] l in
    go [] x
  
  let sort_uniq l = combine_uniq @@ build l

  let inter l1 l2 = merge_uniq l1 (sort_uniq l2)
  let ( *> ) = inter
  let pure x = cons x nil

end

let ( *> ) = LazyList.( *> )
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
  | Bound of _value LazyList.t
  | Free of int
  [@@deriving show]

and value = _value_poly uref [@@deriving show]

and context = Mir.expr Dict.t

type stack = value list

type costack = 
  | Real of stack
  | Fake of costack


