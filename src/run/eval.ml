module type S = sig

  open State

  type t

  val (<&>) : t -> (State.t -> State.t) -> t
  val (>>=) : t -> (State.t -> t) -> t
  val (>=>) : (State.t -> t) -> (State.t -> t) -> (State.t -> t)
  val pure : State.t -> t
  val (<|>) : (State.t -> t) -> (State.t -> t) -> (State.t -> t)
  val ( *> ) : (State.t -> t) -> (State.t -> t) -> (State.t -> t)
  val annihilate : 'a -> t
  val cut : t -> t
  val is_empty : t -> bool

end

module LazySearch : S = struct

  open Batteries
  open LazyList.Infix

  open State

  type t = State.t LazyList.t

  let (<&>) x f = LazyList.map f x
  let (>>=) x f = x <&> f |> LazyList.concat
  let (>=>) f g x = f x >>= g
  let pure a = LazyList.(cons a nil)
  let (<|>) f g x = f x ^@^ g x
  let ( *> ) x c y = x y >>= fun _ -> c y
  let annihilate _ = LazyList.nil
  let cut x = match LazyList.get x with
    | Some (h, _) -> pure h
    | None -> LazyList.nil

  let is_empty = LazyList.is_empty

end
