type 'a t = 'a * int * int [@@deriving show]

val real : 'a -> 'a t -> 'a t
val fake : 'a t -> 'a t
val shift : 'a t -> 'a option * 'a t
val origin : 'a -> 'a t
val height : 'a t -> int
