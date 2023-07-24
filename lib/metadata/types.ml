open! Batteries
open Printf
open Unify
open Ucommon

module DetConst = struct
  type t = bool * bool
  let detmap f (b1, b2) (c1, c2) = f b1 c1, f b2 c2
  let and_const = detmap (&&)
  let xor_const = detmap @@ fun a b -> 
    (a || b) && not (a && b)
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
and Stack : Ulist.UNIF_LIST with type u = Value.t = Ulist.Make(Value)
and Costack : Ulist.UNIF_LIST with type u = Stack.t = struct
  include Ulist.Make(Stack)
  let rec pretty out = uget %> function
    | UCons (u, us) -> 
      pretty out us;
      fprintf out " | "; Stack.pretty out u
    | USeq j -> fprintf out "%d+" j
    | UNil -> fprintf out "$"
end
and Fn : sig
  include UNIFIABLE with type t = Costack.t * Costack.t * Det.t * Det.t
  val ge : t -> t -> bool
  val eq : t -> t -> bool
  val gen : t -> t
end = struct
  type t = Costack.t * Costack.t * Det.t * Det.t
  type memo = unit
  let memo () = ()
  let refresh_memo () = ()
  let unify (c1, c2, x1, y1) (d1, d2, x2, y2) = 
    Costack.unify c1 d1;
    Costack.unify c2 d2;
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
  let atleast m (c1, c2, x1, x2) (d1, d2, y1, y2) = 
    Costack.atleast m c1 d1
    && Costack.atleast m c2 d2
    && Det.atleast m x1 y1
    && Det.atleast m x2 y2
  let ge f1 f2 = atleast (Matcher.mk ()) f1 f2
  let eq f1 f2 = ge f1 f2 && ge f2 f1
  let gen = generalize ()
end

let fresh () = Costack.ufresh (), Costack.ufresh (), Det.bfresh (), Det.bfresh ()
