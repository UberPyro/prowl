type t

exception OccursError of string

val name : string -> t
val num : int -> t
val fresh : unit -> t
val refresher : unit -> < freshen : t -> t >

val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
