open! Batteries

type 'a t [@@deriving show]

val unil : unit -> 'a t
val ucons : 'a -> 'a t -> 'a t
val useq : 'a -> 'a t
val ufront : 'a t -> ('a option, 'a * 'a t) Either.t

val umap : ?g:('a -> 'a) -> ('a -> 'a) -> 'a t -> 'a t
val uiter : ?g:('a -> unit) -> ('a -> unit) -> 'a t -> unit

val unite : ('a -> 'a -> unit) -> 'a t -> 'a t -> unit
