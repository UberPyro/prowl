type t [@@deriving show, eq, ord]

exception OccursError of string

val name : string -> t
val num : int -> t
val fresh : unit -> t
val refresher : unit -> < freshen : t -> t >
