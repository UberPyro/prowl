open! Batteries

type 'a t = 'a * int * int [@@deriving show]

val real : 'a -> 'a t -> 'a t
val fake : 'a t -> 'a t
val shift : 'a t -> ('a t, 'a * int) Either.t
val origin : 'a -> 'a t
val height : 'a t -> int

val assume : 'a t -> 'a * int
val designate : 'a -> int -> 'a t
val nk : int -> 'a t -> 'a t option
val deepen : int -> 'a t -> 'a t
