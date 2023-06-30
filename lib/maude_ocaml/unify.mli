open! Batteries

type fmod
type term

val parse_mod : string -> string -> fmod
val parse_term : fmod -> string -> term
val obscure_term : Maude.c_enum_value Swig.c_obj_t -> term

val unify : fmod -> term -> term -> (string * term) list list

val show_term : term -> string
val expose_term : term -> Maude.c_enum_value Swig.c_obj_t

type symbol

val mk_symmap : fmod -> (string, symbol) Map.t
val build : string -> (string, symbol) Map.t -> term list -> term
val break : term -> string * term list
