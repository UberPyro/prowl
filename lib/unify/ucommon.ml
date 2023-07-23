open! Batteries
open Uref

exception UnifError of string

let pp_uref fmt x y = fmt x (uget y)

include Uref

module type UNIFIABLE = sig
  type t
  type memo
  val unify : t -> t -> unit
  val occurs : int -> t -> unit
  val generalize : memo -> t -> t
  val refresh_memo : unit -> unit
  val memo : unit -> memo
  val pretty : 'a BatInnerIO.output -> t -> unit
end
