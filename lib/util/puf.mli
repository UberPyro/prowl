open! Batteries

type 'a t

val merge : ('a -> 'a -> 'a) -> int -> int -> 'a t -> 'a t
val empty : 'a t
val add : int -> 'a-> 'a t -> 'a t
