open! Batteries

type 'a t

val merge : int -> int -> 'a t -> 'a t
val empty : 'a t
val add : int -> 'a t -> 'a t
val get : int -> 'a t -> 'a option
val set : int -> 'a -> 'a t -> 'a t
