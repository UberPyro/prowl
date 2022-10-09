open Batteries
open Uref

exception Type_error of string

let pp_uref f z y = f z (uget y)

module type SEQ = sig

end

module rec Val : sig

end = struct

end and Seq : functor () -> SEQ = functor () -> struct

end

and Stack : SEQ = Seq ()
and Costack : SEQ = Seq ()
