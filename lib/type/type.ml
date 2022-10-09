open Batteries
open Uref

exception Type_error of string

let pp_uref f z y = f z (uget y)

module type COUNTER = sig
  val count : int ref
  val fresh : unit -> int
end

module Counter () = struct
  let count = ref (-1)
  let fresh () = incr count; !count
end

module type UNIFIABLE = sig
  type t
  val unify : t -> t -> unit
end

module type S = sig

  module Seq : functor (U : UNIFIABLE) -> sig
    include COUNTER
    type t = _t uref
    and _t = 
      | Push of t * U.t
      | Empty of int
    val unify : t -> t -> unit
  end

  module rec Var : sig
    include COUNTER
    type t = _t uref
    and _t = 
      | Var of int
      | Mono of string
      | Duo of string * Costack.t * Costack.t
    val unify : t -> t -> unit
  end

  and Stack : sig
    include COUNTER
    type t = _t uref
    and _t = 
      | Push of t * Var.t
      | Empty of int
    val unify : t -> t -> unit
  end

  and Costack : sig
    include COUNTER
    type t = _t uref
    and _t = 
      | Push of t * Stack.t
      | Empty of int
    val unify : t -> t -> unit
  end

end

module rec T : S = struct

  module Seq (U : UNIFIABLE) = struct
    include Counter ()

    type t = _t uref
    and _t = 
      | Push of t * U.t
      | Empty of int

    let rec unify r = 
      unite ~sel:begin curry @@ function
        | Empty _ as s, Empty _ -> s
        | Push _ as s, Empty _ | Empty _, (Push _ as s) -> s
        | Push (a, t) as s, Push (b, u) -> 
          U.unify t u; 
          unify a b; 
          s
    end r
  end

  module Stack   = Seq (T.Var)
  module Costack = Seq (Stack)

  module Var = struct
    include Counter ()

    type t = _t uref
    and _t = 
      | Var of int
      | Mono of string
      | Duo of string * Costack.t * Costack.t
    
    let unify (r : t) : t -> unit = 
      unite ~sel:begin curry @@ function
        | Duo (s, i0, o0) as q, Duo (t, i1, o1) when s = t -> 
          Costack.unify i0 i1; 
          Costack.unify o0 o1; 
          q
        | v, Var _ | Var _, v -> v
        | _, _ -> 
          raise @@ Type_error begin
            Printf.sprintf "Error Usg Todo"
          end
      end r
  end

end
