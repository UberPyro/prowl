open Batteries
open Uref

exception Type_error of string

let pp_uref f z y = f z (uget y)

module type UNIFIABLE = sig
  type t [@@deriving show]
  val unify : t -> t -> unit
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
  end

  and Stack : sig
    type t = _t uref
    and _t = 
      | Push of t * Var.t
      | Empty of int
      [@@deriving show]
    val unify : t -> t -> unit
  end

  and Costack : sig
    type t = _t uref
    and _t = 
      | Push of t * Stack.t
      | Empty of int
      [@@deriving show]
    val unify : t -> t -> unit
  end

end

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
    
  end

end

include T
