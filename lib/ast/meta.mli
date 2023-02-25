type t

val name : string -> t
val num : int -> t
val uniq : unit -> t

val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
