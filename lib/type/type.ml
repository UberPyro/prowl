open Batteries
open Uref

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
      | Name of int * param list
    and param = 
      | Val of Var.t
      | Del of Costack.t * Costack.t
      [@@deriving show]
    val unify : t -> t -> unit
    val fresh : unit -> t
    val refresh : int HT.t -> t -> t
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
      | Name of int * param list
    and param = 
      | Val of t
      | Del of Costack.t * Costack.t
      [@@deriving show]
    
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
  val var_mismatch : int * T.Var.param list -> 
    int * T.Var.param list -> 'a
end = struct
  exception Unification of string

  let var_mismatch (s0, _) (s1, _) = 
    raise @@ Unification begin Printf.sprintf
      "Cannot unify incompatible types [%d] and [%d]."
      s0 s1
  end

end

include T

(* update this to include implicits? *)
type t = Costack.t * Costack.t

let lit l : t = 
  let c = Costack.fresh () in
  let s = Stack.fresh () in
  Costack.push s c, Costack.push (Stack.push l s) c

let monoid m : t = 
  let c = Costack.fresh () in
  let s = Stack.fresh () in
  Costack.push (s |> Stack.push m |> Stack.push m) c, 
  Costack.push (s |> Stack.push m) c

let endo e : t = 
  let c = Costack.fresh () in
  let s = Stack.fresh () in
  Costack.push (Stack.push e s) c
  |> fun x -> x, x

let free () : t = 
  let mk () = 
    Costack.push
      (Stack.push (Var.fresh ()) (Stack.fresh ()))
      (Costack.fresh ()) in
  mk (), mk ()

let tyint = next ()
let tyfloat = next ()
let tychar = next ()
let tystring = next ()
let tyquote = next ()
let tylist = next ()
let tymap = next ()

let mono id = Var.Name (id, []) |> uref |> lit
let duo id = 
  Var.Name (id, [Val (uref @@ Var.Var (next ()))])
  |> uref |> lit
