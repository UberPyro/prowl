open Batteries
open Uref

(* module type Unifier = sig
  type 'a t
  val unify : ('a -> 'a -> unit) -> 'a t -> 'a t -> unit
end *)

module Var = struct
  let count = ref (-1)
  let fresh () = incr count; !count

  type 'a t = 'a _t uref
  and 'a _t = 
    | Mono of mono
    | Var of int
    | Duo of duo * 'a * 'a
  and mono = Int | Char
  and duo = Quote | List
  
  let rec unify f r = 
    unite ~sel:begin fun t0 u0 -> match t0, u0 with
      | Duo (d0, i0, o0) as g, Duo (d1, i1, o1) when d0 = d1 -> 
        f i0 i1; 
        f o0 o1; 
        g
      | x, Var _ | Var _, x -> x
      | x, y when x = y -> x
      | _, _ -> 
        Printf.sprintf "type error" |> failwith
    end r
end

module MakeSeq () = struct
  let count = ref (-1)
  let fresh () = incr count; !count

  type 'a t = 'a _t uref
  and 'a _t = 
    | Seq of 'a * 'a t
    | Bot of int
  
  let rec unify f r = 
    unite ~sel:begin fun a0 b0 -> match a0, b0 with
      | Bot _ as s, Bot _ -> s
      | Seq _ as s, Bot _ | Bot _, (Seq _ as s) -> s
      | Seq (t, a) as s, Seq (u, b) -> 
        f t u; 
        unify f a b; 
        s
    end r
end

module Stack = MakeSeq ()
module Costack = MakeSeq ()

type t = T of t Var.t Stack.t Costack.t

let _unify f (T u) (T v) = (Costack.unify (Stack.unify (Var.unify f))) u v
let rec unify u v = _unify unify u v
