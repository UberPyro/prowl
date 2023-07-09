open! Batteries

type t

val mk_intern : unit -> t
val intern : t -> string -> int
val get_int : t -> string -> int
val get_str : t -> int -> string
