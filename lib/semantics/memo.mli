open System

type t

val mk_memo : unit -> t
val freshen_value : t -> value -> value
val freshen_stack : t -> stack -> stack
val freshen_costack : t -> costack -> costack
val freshen_mode : t -> Umode.ubool -> Umode.ubool
