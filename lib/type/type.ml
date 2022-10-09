open BatUref

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

module type Mono = sig
  type t
end

module type S = sig

  module Seq : functor (M : Mono) -> sig
    include COUNTER
    type t = _t uref
    and _t = 
      | Push of t * M.t
      | Empty of int
  end

  module rec Var : sig
    include COUNTER
    type t = _t uref
    and _t = 
      | Var of int
      | Mono of string
      | Duo of string * Costack.t * Costack.t
  end

  and Stack : sig
    include COUNTER
    type t = _t uref
    and _t = 
      | Push of t * Var.t
      | Empty of int
  end

  and Costack : sig
    include COUNTER
    type t = _t uref
    and _t = 
      | Push of t * Stack.t
      | Empty of int
  end

end

module rec T : S = struct

  module Seq (M : Mono) = struct
    include Counter ()
    type t = _t uref
    and _t = 
      | Push of t * M.t
      | Empty of int
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
  end

end
