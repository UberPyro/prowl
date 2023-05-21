type 'a t [@@deriving show]

val push : 'a -> 'a t -> 'a t
val push2 : 'a -> 'a -> 'a t -> 'a t
val pop : 'a t -> 'a * 'a t
val pop2 : 'a t -> 'a * 'a * 'a t
val of_list : 'a list -> 'a t
val bottom : 'a t
val len : 'a t -> int
