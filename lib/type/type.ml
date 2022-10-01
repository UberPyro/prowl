open Batteries
open Uref

let count = ref (-1)
let fresh () = incr count; !count

module MakeStack () = struct
  let count = ref (-1)
  let fresh () = incr count; !count

  type 'a var = 'a _var uref
  and 'a _var = 
    | Seq of 'a uref * 'a var
    | Bot of int
end

module Stack = MakeStack ()
module Costack = MakeStack ()
