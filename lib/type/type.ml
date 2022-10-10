open Batteries
open Uref

exception Type_error of string

let pp_uref f z y = f z (uget y)

module HT = Hashtbl.Make(struct
  type t = int
  let hash = Hashtbl.hash
  let equal = (=)
end)

let find_memo rf k ht = 
  match HT.find_option ht k with
  | Some v -> v
  | None -> rf ()

let unew f = uref % f % uget

module type UNIFIABLE = sig
  type t [@@deriving show]
  val unify : t -> t -> unit
  val fresh : unit -> t
  val refresh : int HT.t -> t -> t
end

module type S = sig

  module rec Var : sig
    type t = _t uref
    and _t = 
      | Var of int
      | Mono of string
      | Duo of string * Costack.t * Costack.t
      [@@deriving show]
    val unify : t -> t -> unit
    val fresh : unit -> t
    val refresh : int HT.t -> t -> t
  end

  and Stack : sig
    type t = _t uref
    and _t = 
      | Push of t * Var.t
      | Empty of int
      [@@deriving show]
    val unify : t -> t -> unit
    val fresh : unit -> t
    val refresh : int HT.t -> t -> t
    val push : Var.t -> t -> t
  end

  and Costack : sig
    type t = _t uref
    and _t = 
      | Push of t * Stack.t
      | Empty of int
      [@@deriving show]
    val unify : t -> t -> unit
    val fresh : unit -> t
    val refresh : int HT.t -> t -> t
    val push : Stack.t -> t -> t
  end

end

let count = ref (-1)
let next () = incr count; !count

module Seq (M : UNIFIABLE) = struct

  type t = _t uref
  and _t = 
    | Push of t * M.t
    | Empty of int
    [@@deriving show]

  let rec unify r = 
    unite ~sel:begin curry @@ function
      | Empty _ as s, Empty _ -> s
      | Push _ as s, Empty _ | Empty _, (Push _ as s) -> s
      | Push (a, t) as s, Push (b, u) -> 
        M.unify t u; 
        unify a b; 
        s
  end r

  let fresh () = 
    uref @@ Empty (next ())
  
  let rec refresh c = 
    unew @@ function
      | Empty r -> Empty (find_memo next r c)
      | Push (s, v) -> Push (refresh c s, M.refresh c v)
  
  let push s v = 
    uref @@ Push (v, s)

end

module rec T : S = struct

  module Stack   = Seq (T.Var)
  module Costack = Seq (Stack)

  module Var = struct

    type t = _t uref
    and _t = 
      | Var of int
      | Mono of string
      | Duo of string * Costack.t * Costack.t
      [@@deriving show]
    
    let unify (r : t) : t -> unit = 
      unite ~sel:begin curry @@ function
        | Duo (s, i0, o0) as q, Duo (t, i1, o1) when s = t -> 
          Costack.unify i0 i1; 
          Costack.unify o0 o1; 
          q
        | v, Var _ | Var _, v -> v
        | u, v -> 
          raise @@ Type_error begin
            Printf.sprintf
              "Type [%s] not compatible with [%s]"
              (show__t u)
              (show__t v)
          end
      end r

    let fresh () = 
      uref @@ Var (next ())
    
    let refresh c = 
      unew @@ function
        | Var r -> Var (find_memo next r c)
        | Mono s -> Mono s
        | Duo (s, c1, c2) -> 
          Duo (s, Costack.refresh c c1, Costack.refresh c c2)
    
  end

end

include T

let lit l = 
  let c = Costack.fresh () in
  let s = Stack.fresh () in
  Costack.push s c, Costack.push (Stack.push l s) c

let monoid m = 
  let c = Costack.fresh () in
  let s = Stack.fresh () in
  Costack.push (s |> Stack.push m |> Stack.push m) c, 
  Costack.push (s |> Stack.push m)

let endo e = 
  let c = Costack.fresh () in
  let s = Stack.fresh () in
  Costack.push (Stack.push e s) c
  |> fun x -> x, x

let ascr node ty = node, object
  method ty = ty
end
