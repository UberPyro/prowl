type 'a t

val unite : 'a -> 'a -> 'a t -> 'a t
val empty : 'a t
val add : 'a -> 'a t -> 'a t
