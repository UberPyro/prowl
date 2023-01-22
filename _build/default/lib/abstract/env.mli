open Semantics

exception Unbound_variable of string

type t

val empty : t

val get : string -> t -> costack * costack
val set : string -> costack * costack -> t -> t
val promote : string -> t -> t
