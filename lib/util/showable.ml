open! Batteries

module type S = sig
  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Finite = struct
  type t = int [@@deriving show]
end
