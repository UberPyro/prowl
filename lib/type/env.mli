open Type

type t

val empty : t
val get : string -> t -> costack * costack
val set : string -> costack * costack -> t -> t
val promote : string -> t -> t

val init : string -> t -> t
val unite : string -> var -> t -> unit
val ret : string -> t -> var
