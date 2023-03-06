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

module Op(X : S) = struct
  include X
  let meet = X.join
  let join = X.meet
end

module Pro(X : S)(Y : S) = Bi(Op(X))(Y)

module Segmented = struct
  type t = ISet.t

  let join = ISet.union
  let meet = ISet.inter

  module I = struct
    type t = (int * int) list [@@deriving show]
  end
  let pp fmt = ISet.ranges %> I.pp fmt
  let show = ISet.ranges %> I.show
end

module Tagged(X : S) = struct
  type t = (string, X.t) Map.t

  let meet : t -> t -> t = Map.intersect X.meet
  let join : t -> t -> t = Map.union_stdlib (fun _ x y -> Some (X.join x y))

  module P = struct
    type t = (string * X.t) list [@@deriving show]
  end
  let pp fmt = Map.bindings %> P.pp fmt
  let show = Map.bindings %> P.show
end

module TSeg = Tagged(Segmented)
