open! Batteries

open Metadata
open Unify
open Ucommon
open Ulist

include Types

let rec unify v0 = Usyn.unify
  (Ueither.unify Uword.unify (fun x y -> unify_fn (uget x) (uget y)))
  (Ueither.occurs Uword.occurs occurs_fn) v0
and occurs_val i v0 = Usyn.occurs (Ueither.occurs Uword.occurs occurs_fn) i v0
and unify_stack s0 = Ulist.unify unify occurs_val s0
and occurs_stack k = Ulist.occurs occurs_val k
and (=?=) c0 = Ulist.unify unify_stack occurs_stack c0
and occurs_costack k = Ulist.occurs occurs_stack k

and unify_fn (Fn (c1, c2)) (Fn (d1, d2)) = 
  c1 =?= d1;
  c2 =?= d2

and occurs_fn k (Fn (c1, c2)) = 
  occurs_costack k c1;
  occurs_costack k c2

let mk_init_costack () = ucons (unil ()) (unil ())
let mk_end_costack () = ucons (ufresh ()) (unil ())
let mk_poly_costack () = ucons (ufresh ()) (ufresh ())
let (@>) v_ = map_hd (ucons (uref v_))
let mk_var () = Usyn.uvar ()
let (@@>) v = map_hd (ucons v)
let (@>>) s = ucons s
let (-?-) c0 c1 = 
  c0 =?= mk_poly_costack ();
  c1 =?= mk_poly_costack ();
  match uget c0, uget c1 with
  | UCons (u, _), UCons (v, _) -> unify_stack u v
  | _ -> Invalid_argument "-?-" |> raise
