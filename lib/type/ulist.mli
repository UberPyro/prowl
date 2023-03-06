open! Batteries

open Meta

type 'a t [@@deriving show] 
type 'a ulist = 'a t [@@deriving show]

val unil : unit -> 'a t
val ucons : 'a -> 'a t -> 'a t
val useq : Var.t -> 'a t
val ufresh : unit -> 'a t

val unite : ?fvars:< occurs : Var.t -> unit > ->
  ('a -> 'a -> unit) -> 'a t -> 'a t -> unit

val pp_uref : ('a -> 'b -> 'c) -> 'a -> 'b Uref.t -> 'c
