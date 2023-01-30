(* Unifiable Lists *)
open! Batteries

type ('a, 'b) t

val unite_seq : sel:('a -> 'a -> unit) -> ('a, 'b) t -> ('a, 'b) t -> unit
val make : 'a -> 'b list -> ('b, 'a) t
val push : ('a, 'b) t -> 'a -> ('a, 'b) t
val to_list : ('a, 'b) t -> 'a list * 'b option
val iter : ('a -> unit) -> ('b -> unit) -> ('a, 'b) t -> unit
val remap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
val map_top : ('a -> 'a) -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
val get_top : ('a, 'b) t -> 'a
val get_top2 : ('a, 'b) t -> 'a * 'a

val show : (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> ('a, 'b) t -> string
val pp : (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> Format.formatter -> ('a, 'b) t -> unit
val pp_uref : ('a -> 'b -> 'c) -> 'a -> 'b Uref.t -> 'c
