open! Batteries
open Uref
open Printf

type uerr = 
  | DifferentNames of string * string
  | DifferentArity of int * int
  | Occurs
  | TermHead of string
  | DifferentSizes
  | TerminatedSequence
  | OccursSequence of int * string
  | DifferentBooleans of string * string
  | TooGeneralSpec of string * string
  | UnboundVariable of string
  | UnboundUVar of string
  | UnboundSVar of string
exception UnifError of uerr

let pp_err out = begin function
  | DifferentNames (s1, s2) -> 
    fprintf out "Cannot unify distinct names [%s] and [%s]" s1 s2
  | DifferentArity (i1, i2) -> 
    fprintf out "Cannot unify terms of arity [%d] and [%d]" i1 i2
  | TermHead s -> fprintf out "Cannot unify term head [%s] with term contents" s
  | Occurs -> fprintf out "Cannot unify a variable with syntax that contains it"
  | DifferentSizes -> fprintf out "Cannot unify differently sized sequences"
  | TerminatedSequence -> fprintf out "Cannot extend terminated difference list"
  | OccursSequence (i, s) -> fprintf out
    "Cannot unify a variable [%d] with a sequence [%s] that contains it" i s
  | DifferentBooleans (s1, s2) -> fprintf out
    "Cannot unify booleans [%s] and [%s]" s1 s2
  | TooGeneralSpec (s1, s2) -> fprintf out
    "The inferred type [%s] is less general than the annotation [%s]" s1 s2
  | UnboundVariable s -> fprintf out "Cannot find unbound variable [%s]" s
  | UnboundUVar s -> fprintf out "Cannot find unbound unification variable [%s]" s
  | UnboundSVar s -> fprintf out "Cannot find unbound stack variable [%s]" s
end %> fun () -> fprintf out "\n"

let pp_uref fmt x y = fmt x (uget y)

include Uref

module Matcher : sig
  type t
  val check : t -> int -> int -> bool
  val mk : unit -> t
end = struct
  module IM = Hashtbl.Make(struct
    include Hashtbl
    include Int
  end)
  type t = int IM.t
  let check m i j = 
    IM.find_option m i |> Option.map_default_delayed
      ((=) j)
      (fun () -> IM.add m i j; true)
  let mk () = IM.create 32
end

module type UNIFIABLE = sig
  type t
  type memo
  val unify : t -> t -> unit
  val occurs : int -> t -> unit
  val generalize : memo -> t -> t
  val refresh_memo : unit -> unit
  val memo : unit -> memo
  val pretty : 'a BatInnerIO.output -> t -> unit
  val atleast : Matcher.t -> t -> t -> bool
end

let pstr f x = 
  let out = IO.output_string () in
  f out x;
  IO.close_out out
