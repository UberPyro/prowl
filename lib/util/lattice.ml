open! Batteries

module type S = sig
  type t [@@deriving show]

  val join : t -> t -> t
  val meet : t -> t -> t
end

module Logical = struct
  type t = bool [@@deriving show]

  let join = (||)
  let meet = (&&)
  let make = Fun.id
end

module Bi(X : S)(Y : S) = struct
  type t = X.t * Y.t [@@deriving show]

  let bi f g (x1, y1) (x2, y2) = f x1 x2, g y1 y2
  let join = bi X.join Y.join
  let meet = bi X.meet Y.meet
end

module Opposite(X : S) = struct
  include X
  let meet = X.join
  let join = X.meet
end

module Pro(X : S)(Y : S) = Bi(Opposite(X))(Y)

module Lexical(X : S)(Y : S) = struct  (* As in lexical sort *)
  include Bi(X)(Y)
  let bi_lex f g u v = match u, v with
    | (x1, y1), (x2, y2) when x1 = x2 -> x1, g y1 y2
    | (x1, y1), (x2, y2) -> 
      let x' = f x1 x2 in
      x', if x' = x1 then y1 else y2
  let meet = bi_lex X.meet Y.meet
  let join = bi_lex X.join Y.join
end

module Poly(S : Showable.S) = struct
  include S
  let join = max
  let meet = min
end

module Finite = Poly(Showable.Finite)

module Location = Lexical(Finite)(Finite)
module Span = Pro(Location)(Location)

module Det = Bi(Logical)(Logical)
module Mode = Bi(Det)(Det)
