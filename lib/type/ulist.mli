open! Batteries

open Meta

type 'a t [@@deriving show] 
type 'a ulist = 'a t [@@deriving show]

val unil : unit -> 'a t
val ucons : 'a -> 'a t -> 'a t
val useq : Var.t -> 'a t
val ufresh : unit -> 'a t

val unite : ('a -> 'a -> unit) -> (Var.t -> 'a -> unit) -> 'a ulist -> 'a ulist -> unit
val occurs : (Var.t -> 'a -> unit) -> Var.t -> 'a ulist -> unit

val remap : ('a -> 'b) -> (Var.t -> Var.t) -> 'a ulist -> 'b ulist
val uiter : ?g:(Var.t -> unit) -> ('a -> unit) -> 'a ulist -> unit

val pp_uref : ('a -> 'b -> 'c) -> 'a -> 'b Uref.t -> 'c
val assert_exn : exn -> 'a -> 'a -> unit
