open Batteries

module StrKey = struct
  type t = string
  let compare = compare
end

module Dict = Map.Make(StrKey)

module rec Value : sig

  exception ExpectedType of string

  type t = 
    | VInt of int
    | VStr of string
    | VPair of Capture.t * Capture.t
    | VLeft of Capture.t
    | VRight of Capture.t
    | VImm of Capture.t
    | VUnit
    | VMod of Module.t
    | VImpl of Capture.t
    | VImplMod
    | VBuiltin of string
  
  val to_int : t -> int
  val to_str : t -> string
  val to_pair : t -> Capture.t * Capture.t
  val to_eith : t -> Capture.t
  val to_cap : t -> Capture.t
  val to_unit : t -> unit

end = struct

  exception ExpectedType of string

  type t = 
    | VInt of int
    | VStr of string
    | VPair of Capture.t * Capture.t
    | VLeft of Capture.t
    | VRight of Capture.t
    | VImm of Capture.t
    | VUnit
    | VMod of Module.t
    | VImpl of Capture.t
    | VImplMod
    | VBuiltin of string
  
  let to_int = function VInt i -> i | _ -> raise (ExpectedType "Int")
  let to_str = function VStr s -> s | _ -> raise (ExpectedType "Str")
  let to_pair = function VPair (c1, c2) -> c1, c2 | _ -> raise (ExpectedType "Pair")
  let to_eith = function VLeft c | VRight c -> c | _ -> raise (ExpectedType "Eith")
  let to_cap = function VImm c -> c | _ -> raise (ExpectedType "Capture")
  let to_unit = function VUnit -> () | _ -> raise (ExpectedType "Unit")

end

and Type : sig
  type t
end = struct
  type t
end

and Module : sig

  type t

  val def : string -> Ast.e -> t -> t
  val acc : string -> t -> Ast.e
  val deft : string -> string list * Ast.ty option -> t -> t
  val acct : string -> t -> string list * Ast.ty option
  val defi : string -> Ast.e -> t -> t
  val acci : string -> t -> Ast.e

  val set : string -> Value.t -> t -> t
  val get : string -> t -> Value.t
  val sett : string -> Type.t -> t -> t
  val gett : string -> t -> Type.t
  val ins : string -> Module.t -> t -> t
  val dump : string -> t -> Module.t list

end = struct

  type t = {
    def : Ast.e Dict.t;
    ty : (string list * Ast.ty option) Dict.t;
    impl : Ast.e Dict.t;
    c : Context.t
  }

  let def k v m = {m with def = Dict.add k v m.def}
  let acc k m = Dict.find k m.def

  let deft k v m = {m with ty = Dict.add k v m.ty}
  let acct k m = Dict.find k m.ty

  let defi k v m = {m with impl = Dict.add k v m.impl}
  let acci k m = Dict.find k m.impl

  let set k v m = {m with c = Context.set k v m.c}
  let get k m = Context.get k m.c

  let sett k t m = {m with c = Context.sett k t m.c}
  let gett k m = Context.gett k m.c

  let ins k i m = {m with c = Context.ins k i m.c}
  let dump k m = Context.dump k m.c

end

and Capture : sig

  type t

  val ast : t -> Ast.e
  val make : Ast.e -> Context.t -> t
  val of_st : Ast.e -> State.t -> t
  val c : t -> Context.t

  val set : string -> Value.t -> t -> t
  val get : string -> t -> Value.t
  val sett : string -> Type.t -> t -> t
  val gett : string -> t -> Type.t
  val ins : string -> Module.t -> t -> t
  val dump : string -> t -> Module.t list

end = struct

  type t = {
    e : Ast.e;
    c : Context.t;
  }

  let ast a = a.e
  let make e c = {e; c}
  let of_st e st = {e; c = State.c st}
  let c st = st.c

  let set k v a = {a with c = Context.set k v a.c}
  let get k a = Context.get k a.c

  let sett k t a = {a with c = Context.sett k t a.c}
  let gett k a = Context.gett k a.c

  let ins k i a = {a with c = Context.ins k i a.c}
  let dump k a = Context.dump k a.c

end

and State : sig

  type t
  type stack = Value.t list

  exception Underflow of t

  val s : t -> stack
  val c : t -> Context.t

  val pop : t -> Value.t * t
  val pop2 : t -> Value.t * Value.t * t
  val push : Value.t -> t -> t
  val push2 : Value.t -> Value.t -> t -> t

  val set : string -> Value.t -> t -> t
  val get : string -> t -> Value.t
  val sett : string -> Type.t -> t -> t
  val gett : string -> t -> Type.t
  val ins : string -> Module.t -> t -> t
  val dump : string -> t -> Module.t list

  module Infix : sig

    val (!:) : t -> Value.t * t
    val (!::) : t -> Value.t * Value.t * t
    val (>:) : Value.t -> t -> t
    val (>::) : Value.t -> Value.t -> t -> t
  
    val (<--) : string -> Value.t * t -> t
    val (-->) : string -> t -> Value.t
    val (<==) : string -> Type.t * t -> t
    val (==>) : string -> t -> Type.t
    val (<<-) : string -> Module.t * t -> t
    val (->>) : string -> t -> Module.t list

  end

end = struct

  type t = {
    s : stack;
    c : Context.t;
  }
  and stack = Value.t list

  exception Underflow of t

  let s st = st.s
  let c st = st.c

  let pop = function
    | ({s = h :: s; _} as st) -> h, {st with s}
    | st -> raise (Underflow st)
  
  let pop2 = function
    | ({s = h1 :: h2 :: s; _} as st) -> h1, h2, {st with s}
    | st -> raise (Underflow st)
  
  let push v st = {st with s = v :: st.s}
  let push2 v1 v2 st = {st with s = v1 :: v2 :: st.s}

  let set k v st = {st with c = Context.set k v st.c}
  let get k st = Context.get k st.c

  let sett k t st = {st with c = Context.sett k t st.c}
  let gett k st = Context.gett k st.c

  let ins k i st = {st with c = Context.ins k i st.c}
  let dump k st = Context.dump k st.c

  module Infix = struct

    let (!:) = pop
    let (!::) = pop2
    let (>:) = push
    let (>::) = push2

    let (<--) k (v, st) = set k v st
    let (-->) = get
    let (<==) k (t, st) = sett k t st
    let (==>) = gett
    let (<<-) k (i, st) = ins k i st
    let (->>) = dump

  end

end

and Context : sig

  type t = {
    v : Value.t Dict.t;  (* Value Context *)
    t : Type.t Dict.t;  (* Type Context *)
    i : Module.t list Dict.t;  (* Implicits *)
  }

  val set : string -> Value.t -> t -> t
  val get : string -> t -> Value.t

  val sett : string -> Type.t -> t -> t
  val gett : string -> t -> Type.t

  val ins : string -> Module.t -> t -> t
  val dump : string -> t -> Module.t list

end = struct

  type t = {
    v : Value.t Dict.t;
    t : Type.t Dict.t;
    i : Module.t list Dict.t;
  }

  let set k v c = {c with v = Dict.add k v c.v}
  let get k c = Dict.find k c.v

  let sett k t c = {c with t = Dict.add k t c.t}
  let gett k c = Dict.find k c.t

  let ins k i c = {c with i = Dict.add k (i :: Dict.find k c.i) c.i}
  let dump k c = Dict.find k c.i

end
