open! Batteries
open Uref

exception UnifError of string

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
