open! Batteries

type 'a t
type 'a puf

val search : int -> 'a puf -> int * 'a
val search_all : int -> 'a t -> (int * 'a) list
val merge : ('a -> 'a -> 'a puf -> ('a * 'a puf) list) -> int -> int -> 'a puf -> 'a t
val (<|>) : 'a t -> 'a t -> 'a t
val pure : 'a puf -> 'a t
val empty : 'a puf
val add_det : int -> 'a -> 'a puf -> 'a puf
val add_nondet : int -> 'a -> 'a t -> 'a t
val (let+) : 'a t -> ('a puf -> 'a puf) -> 'a t
val (let*) : 'a t -> ('a puf -> 'a t) -> 'a t
