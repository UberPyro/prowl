type ('a, 'b) t

val unite_seq : sel:('a -> 'a -> unit) -> ('a, 'b) t -> ('a, 'b) t -> unit
val make : 'a -> 'b list -> ('b, 'a) t
val to_list : ('a, 'b) t -> 'a list * 'b
val iter : ('a -> unit) -> ('b -> 'c) -> ('a, 'b) t -> 'c
