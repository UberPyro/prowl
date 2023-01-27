type t

val make : Lexing.position * Lexing.position -> t
val join : t -> t -> t

val pp : Format.formatter -> t -> unit
val show : t -> string
