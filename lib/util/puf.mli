open! Batteries

type 'a t

val search : 'a -> 'a t -> 'a
val merge : 'a -> 'a -> 'a t -> 'a t
val empty : 'a t
val add : 'a -> 'a t -> 'a t
