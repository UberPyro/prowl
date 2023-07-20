open! Batteries

open Metadata

include Types
module C = Costack
module S = Stack
module V = Value

let mk_init_costack () = C.ucons (S.unil ()) (C.unil ())
let mk_end_costack () = C.ucons (S.ufresh ()) (C.unil ())
let mk_poly_costack () = C.ucons (S.ufresh ()) (C.ufresh ())
let mk_var () = Value.uvar ()
let (@>) v = C.map_hd (S.ucons v)
let (@>>) s = C.ucons s
let (=?=) = C.unify
