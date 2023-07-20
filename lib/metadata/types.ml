open! Batteries
open Printf
open Unify
open Ucommon

module DetConst = struct
  type t = bool * bool
  let detmap f (b1, b2) (c1, c2) = f b1 b2, f c1 c2
  let and_const x y = detmap (&&) x y
  let xor_const x y = detmap (fun a b -> 
    (a || b) && not (a && b)) x y
  let one = true, true
  let zero = false, false
  let to_string = function
    | true, true -> "fn"
    | false, true -> "pt"
    | true, false -> "mt"
    | false, false -> "rl"
end

module Det = Ubool.Make(DetConst)

module rec Value : sig
  include UNIFIABLE
  val usyn : string -> t list -> t
  val uvar : unit -> t
  val uatom : Fn.t -> t
end = struct
  include Usyn.Make(Fn)
  let rec pretty out = uget %> function
    | USyntax ("int", []) -> fprintf out "%s" "z"
    | USyntax ("string", []) -> fprintf out "%s" "str"
    | USyntax (n, []) -> fprintf out "%s" n
    | USyntax ("quote", [a]) -> 
      let[@warning "-8"] UAtom fn = uget a in
      fprintf out "%s" "[";
      Fn.pretty out fn;
      fprintf out "%s" "]"
    | USyntax ("list", [a]) -> 
      let[@warning "-8"] UAtom fn = uget a in
      fprintf out "%s" "{";
      Fn.pretty out fn;
      fprintf out "%s" "}"
    | USyntax (n, us) -> 
      fprintf out "%s(" n;
      begin match us with
        | [] -> ()
        | h :: t -> 
          pretty out h;
          List.iter (fun u -> fprintf out ","; pretty out u) t
      end;
      fprintf out ")"
    | UVar j -> fprintf out "V%d" j
    | UAtom a -> Fn.pretty out a
end
and Stack : sig
  include UNIFIABLE
  val unil : unit -> t
  val ucons : Value.t -> t -> t
  val useq : int -> t
  val ufresh : unit -> t
  val usplit : ?acc:Value.t list -> t -> Value.t list * int option
  val ujoin : t -> Value.t list -> t
  val rebase : t -> t -> t
  val map_hd : (Value.t -> Value.t) -> t -> t
  val upop : t -> Value.t * t
end = Ulist.Make(Value)
and Costack : sig
  include UNIFIABLE
  val unil : unit -> t
  val ucons : Stack.t -> t -> t
  val useq : int -> t
  val ufresh : unit -> t
  val usplit : ?acc:Stack.t list -> t -> Stack.t list * int option
  val ujoin : t -> Stack.t list -> t
  val rebase : t -> t -> t
  val map_hd : (Stack.t -> Stack.t) -> t -> t
  val upop : t -> Stack.t * t
end = struct 
  include Ulist.Make(Stack)
  let rec pretty out = uget %> function
    | UCons (u, us) -> 
      pretty out us;
      fprintf out " | "; Stack.pretty out u
    | USeq j -> fprintf out "%d+" j
    | UNil -> fprintf out "$"
end
and Fn : UNIFIABLE with type t = Costack.t * Costack.t * Det.t * Det.t = struct
  type t = Costack.t * Costack.t * Det.t * Det.t
  type memo = unit
  let memo () = ()
  let refresh_memo () = ()
  let unify (c1, c2, x1, y1) (d1, d2, x2, y2) = 
    Costack.unify c1 c2;
    Costack.unify d1 d2;
    Det.unify x1 x2;
    Det.unify y1 y2
  let occurs i (c1, c2, _, _) = 
    Costack.occurs i c1;
    Costack.occurs i c2
  let generalize () (c1, c2, d1, d2) = 
    Costack.generalize (Costack.memo ()) c1,
    Costack.generalize (Costack.memo ()) c2, 
    Det.generalize (Det.memo ()) d1,
    Det.generalize (Det.memo ()) d2
  let pretty out (c1, c2, d1, d2) = 
    Costack.pretty out c1;
    fprintf out "%s" " -- ";
    Costack.pretty out c2;
    fprintf out "%s" " :: ";
    Det.pretty out d1;
    fprintf out "%s" "/";
    Det.pretty out d2
end

let fresh () = Costack.ufresh (), Costack.ufresh (), Det.bfresh (), Det.bfresh ()
