open! Batteries

type ('a, 'b) t [@@deriving show]
type ('a, 'b) ulist = ('a, 'b) t [@@deriving show]

val unil : unit -> ('a, 'b) t
val ucons : 'a -> ('a, 'b) t -> ('a, 'b) t
val useq : 'a -> ('b, 'a) t
val ufront : ('a, 'b) t -> ('b option, 'a * ('a, 'b) ulist) Either.t

val umap : ?g:('a -> 'a) -> ('b -> 'c) -> ('b, 'a) t -> ('c, 'a) t
val uiter : ?g:('a -> unit) -> ('b -> unit) -> ('b, 'a) t -> unit

val unite : ('a -> 'a -> unit) -> ('a, 'b) t -> ('a, 'b) t -> unit

val pp_uref : ('a -> 'b -> 'c) -> 'a -> 'b Uref.t -> 'c
