open Type

type t

val empty : t
val get : string -> t -> costack * costack
val set : string -> costack * costack -> t -> t
val promote : string -> t -> t

val init : string -> t -> t
val unite : string -> var -> t -> unit
val ret : string -> t -> var

val init_stack : string -> t -> t
val unite_stack : string -> var seq -> t -> unit
val ret_stack : string -> t -> var seq

val init_costack : string -> t -> t
val unite_costack : string -> var seq seq -> t -> unit
val ret_costack : string -> t -> var seq seq

val get_type : string -> t -> var list list
val set_type : string -> var list list -> t -> t
