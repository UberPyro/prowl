type 'a t [@@deriving show]

val empty : 'a t
val pure : 'a -> 'a t
val make : 'a list -> 'a t
val bind_uniq : ('a -> 'b t) -> 'a t -> 'b t
val kleisli : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
val append_uniq : 'a t -> 'a t -> 'a t
val map_uniq : ('a -> 'b) -> 'a t -> 'b t
val plus : ('a -> 'a t) -> 'a -> 'a t
val star : ('a -> 'a t) -> 'a -> 'a t
val mark : ('a -> 'a t) -> 'a -> 'a t
val list : 'a t -> 'a list
