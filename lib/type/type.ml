open Batteries
open Uref

type path = string list * string [@@deriving show]

let pp_uref f z y = f z (uget y)

module HT = Hashtbl.Make(struct
  include Int
  let hash = Hashtbl.hash
end)

let find_memo rf ht = 
  HT.find_option ht
  %> Option.default_delayed rf

let unew f = uget %> f %> uref

module type UNIFIABLE = sig
  type t [@@deriving show]
  val unify : t -> t -> unit
  val fresh : unit -> t
  val refresh : int HT.t -> t -> t
end

module type S = sig

  module rec Var : sig
    type t = _t uref
    and _t = 
      | Var of int
      | Name of path * param list
    and param = 
      | Val of Var.t
      | Del of Costack.t * Costack.t
      [@@deriving show]
    val unify : t -> t -> unit
    val fresh : unit -> t
    val refresh : int HT.t -> t -> t
    val show_name : path -> param list -> string
  end

  and Stack : sig
    type t = _t uref
    and _t = 
      | Push of t * Var.t
      | Empty of int
      [@@deriving show]
    val unify : t -> t -> unit
    val fresh : unit -> t
    val refresh : int HT.t -> t -> t
    val push : Var.t -> t -> t
  end

  and Costack : sig
    type t = _t uref
    and _t = 
      | Push of t * Stack.t
      | Empty of int
      [@@deriving show]
    val unify : t -> t -> unit
    val fresh : unit -> t
    val refresh : int HT.t -> t -> t
    val push : Stack.t -> t -> t
  end

end

let count = ref (-1)
let next () = incr count; !count

module Seq (M : UNIFIABLE) = struct

  type t = _t uref
  and _t = 
    | Push of t * M.t
    | Empty of int
    [@@deriving show]

  let rec unify r = 
    unite ~sel:begin curry @@ function
      | Empty _ as s, Empty _ -> s
      | Push _ as s, Empty _ | Empty _, (Push _ as s) -> s
      | Push (a, t) as s, Push (b, u) -> 
        M.unify t u; 
        unify a b; 
        s
  end r

  let fresh () = 
    uref @@ Empty (next ())
  
  let rec refresh c = 
    unew @@ function
      | Empty r -> Empty (find_memo next c r)
      | Push (s, v) -> Push (refresh c s, M.refresh c v)
  
  let push s v = 
    uref @@ Push (v, s)

end

module rec T : S = struct

  module Stack   = Seq (T.Var)
  module Costack = Seq (Stack)

  module Var = struct

    type t = _t uref
    and _t = 
      | Var of int
      | Name of path * param list
    and param = 
      | Val of t
      | Del of Costack.t * Costack.t
      [@@deriving show]
    
    let show_name u us = 
      Printf.sprintf
        "%s%s"
        begin if List.is_empty us then "" else
          List.map (show_param %> Printf.sprintf "%s ") us
          |> String.concat ""
        end
        (show_path u)
    
    let rec unify (r : t) : t -> unit = 
      unite ~sel:begin curry @@ function
        | Name (u, us), Name (v, vs)  (* differently kinded *)
        when u = v && List.compare_lengths us vs = 0 -> 
          Error.var_mismatch (u, us) (v, vs)
        | Name (u, us) as n, Name (v, vs) when u = v -> 
          List.iter2 begin curry @@ function
            | Val x, Val y -> unify x y
            | Del (i0, o0), Del (i1, o1) -> 
              Costack.unify i0 i1; 
              Costack.unify o0 o1
            | _, _ -> failwith ""
          end us vs;
          n
        | v, Var _ | Var _, v -> v
        | Name (u, us), Name (v, vs) -> 
          Error.var_mismatch (u, us) (v, vs)
      end r

    let fresh () = 
      uref @@ Var (next ())
    
    let rec refresh c = 
      unew @@ function
        | Var r -> Var (find_memo next c r)
        | Name (u, us) -> 
          Name (u, us |> List.map @@ function
            | Val v -> Val (refresh c v)
            | Del (c1, c2) -> 
              Del (Costack.refresh c c1, Costack.refresh c c2)
          )
    
  end

end

and Error : sig
  exception Unification of string
  val var_mismatch : path * T.Var.param list -> 
      path * T.Var.param list -> 'a
end = struct
  exception Unification of string

  let var_mismatch (s0, ps0) (s1, ps1) = 
    raise @@ Unification begin Printf.sprintf
      "Cannot unify incompatible types [%s] and [%s]."
      (T.Var.show_name s0 ps0)
      (T.Var.show_name s1 ps1)
  end

end
