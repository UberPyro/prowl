open Batteries

open Util

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
    | VCap of Capture.t
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
  val to_mod : t -> Module.t

  val show : t -> string

end = struct

  exception ExpectedType of string

  type t = 
    | VInt of int
    | VStr of string
    | VPair of Capture.t * Capture.t
    | VLeft of Capture.t
    | VRight of Capture.t
    | VCap of Capture.t
    | VUnit
    | VMod of Module.t
    | VImpl of Capture.t
    | VImplMod
    | VBuiltin of string
  
  let to_int = function VInt i -> i | _ -> raise (ExpectedType "Int")
  let to_str = function VStr s -> s | _ -> raise (ExpectedType "Str")
  let to_pair = function VPair (c1, c2) -> c1, c2 | _ -> raise (ExpectedType "Pair")
  let to_eith = function VLeft c | VRight c -> c | _ -> raise (ExpectedType "Either")
  let to_cap = function VCap c -> c | _ -> raise (ExpectedType "Capture")
  let to_unit = function VUnit -> () | _ -> raise (ExpectedType "Unit")
  let to_mod = function VMod m -> m | _ -> raise (ExpectedType "Module")

  let enhanced_show_e = function
    | Ast.Int i, _ -> string_of_int i
    | Ast.Str s, _ -> s
    | ex -> Ast.show_e ex

  let show_c = Capture.ast >> enhanced_show_e

  let show = function
    | VInt i -> string_of_int i
    | VStr s -> s
    | VUnit -> "<>"
    | VPair (c1, c2) -> Printf.sprintf "(%s, %s)" (show_c c1) (show_c c2)
    | VLeft c -> Printf.sprintf "(%s;)" (show_c c)
    | VRight c -> Printf.sprintf "(;%s)" (show_c c)
    
    | _ -> failwith "Unimplemented - show"

end

and Type : sig
  type t
end = struct
  type t
end

and Module : sig

  type t

  val make : State.t -> t
  val body : t -> Ast.e Dict.t * (string list * Ast.ty option) Dict.t * Ast.e Dict.t
  val c : t -> Context.t

  val def : string -> Ast.e -> t -> t
  val def_open : t -> t -> t
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
    c : Context.t;
  }

  let make st = {
    def = Dict.empty;
    ty = Dict.empty;
    impl = Dict.empty;
    c = State.c st;
  }

  let body m = m.def, m.ty, m.impl
  let c m = m.c

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

  let def_open mo m = {
    m with
    c = Context.update m.c (Context.of_mod mo)
  }

end

and Capture : sig

  type t

  val ast : t -> Ast.e
  val make : Ast.e -> Context.t -> t
  val of_st : Ast.e -> State.t -> t
  val of_mod : Ast.e -> Module.t -> t
  val c : t -> Context.t
  val init : Ast.e -> t

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
  let of_mod e m = {e; c = Module.c m}
  let c st = st.c
  let init e = {e; c = Context.init}

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

  exception Underflow

  val s : t -> stack
  val c : t -> Context.t
  val init : t
  val merge : Capture.t -> t -> t
  val merge_mod : Module.t -> t -> t
  val update : Capture.t -> t -> t
  val update_mod : Module.t -> t -> t
  val switch : t -> t -> t
  val restack : stack -> t -> t

  val pop : t -> Value.t * t
  val pop2 : t -> Value.t * Value.t * t
  val pop_opt : t -> (Value.t * t) option
  val pop2_opt : t -> (Value.t * Value.t * t) option
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
    val (!?) : t -> (Value.t * t) option
    val (!??) : t -> (Value.t * Value.t * t) option
    val (>:) : Value.t -> t -> t
    val (>::) : Value.t * Value.t -> t -> t
  
    val (<--) : t -> string * Value.t -> t
    val (-->) : t -> string -> Value.t
    val (<==) : t -> string * Type.t -> t
    val (==>) : t -> string -> Type.t
    val (<<-) : t -> string * Module.t -> t
    val (->>) : t -> string -> Module.t list

    val (<-|) : t -> Capture.t -> t
    val (<-<) : t -> Capture.t -> t
    val (<->) : t -> t -> t

  end

end = struct

  type t = {
    s : stack;
    c : Context.t;
  }
  and stack = Value.t list

  exception Underflow

  let s st = st.s
  let c st = st.c
  let merge vi st = {st with c = Capture.c vi}
  let merge_mod m st = {st with c = Module.c m}
  let update vi st = {st with c = Context.update st.c (Capture.c vi)}
  let update_mod m st = {st with c = Context.update st.c (Context.of_mod m)}
  let switch st1 st2 = {st1 with c = st2.c}
  let empty = {s = []; c = Context.empty}
  let init = {empty with c = Context.init}
  let restack s st = {st with s}

  let pop = function
    | ({s = h :: s; _} as st) -> h, {st with s}
    | _ -> raise Underflow
  
  let pop2 = function
    | ({s = h1 :: h2 :: s; _} as st) -> h1, h2, {st with s}
    | _ -> raise Underflow
  
  let pop_opt = function
    | ({s = h :: s; _} as st) -> Some (h, {st with s})
    | _ -> None
  
  let pop2_opt = function
    | ({s = h1 :: h2 :: s; _} as st) -> Some (h1, h2, {st with s})
    | _ -> None
  
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
    let (!?) = pop_opt
    let (!??) = pop2_opt
    let (>:) = push
    let (>::) (v1, v2) = push2 v1 v2

    let (<--) st (k, v) = set k v st
    let (-->) st k = get k st
    let (<==) st (k, t) = sett k t st
    let (==>) st k = gett k st
    let (<<-) st (k, i) = ins k i st
    let (->>) st k = dump k st

    let (<-|) st c = merge c st
    let (<-<) st c = update c st
    let (<->) = switch

  end

end

and Context : sig

  type t = {
    v : Value.t Dict.t;  (* Value Context *)
    t : Type.t Dict.t;  (* Type Context *)
    i : Module.t list Dict.t;  (* Implicits *)
  }

  val empty : t
  val init : t
  val of_mod : Module.t -> t
  val update : t -> t -> t

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

  let empty = {
    v = Dict.empty;
    t = Dict.empty;
    i = Dict.empty;
  }

  let of_mod m = 
    let d, _, _ = Module.body m in {
      v = Dict.map (fun x -> Value.VCap (Capture.of_mod x m)) d
        |> Dict.union (fun _ _ c -> Some c) (Module.c m).v;
      t = Dict.empty;
      i = Dict.empty;
    }

  let set k v c = {c with v = Dict.add k v c.v}
  let get k c = Dict.find k c.v

  let sett k t c = {c with t = Dict.add k t c.t}
  let gett k c = Dict.find k c.t

  let ins k i c = 
    Dict.find_opt k c.i
    |> Option.default []
    |> List.cons i
    |> fun v -> {c with i = Dict.add k v c.i}

  let dump k c = Dict.find k c.i

  let upd y z = Dict.union (fun _ _ x -> Some x) y z
  let updi y z = Dict.union (fun _ x1 x2 -> Some (x1 @ x2)) y z
  let update c1 c2 = {
    v = upd c1.v c2.v;
    t = upd c1.t c2.t;
    i = updi c1.i c2.i;
  }

  let init_v = [
    "+", "add";
    "-", "sub";
    "*", "mul";
    "/", "div"; 
    "**", "exp"; 
  
    "==", "eq"; 
    "/=", "ne"; 
    ">", "gt"; 
    "<", "lt"; 
    ">=", "ge"; 
    "<=", "le"; 
  
    "&", "cat"; 
    "|", "alt"; 
    "|?", "alt-rel"; 
    "|+", "alt-cut"; 
    "&&", "intersect";
  ]
  |> List.map (fun (a, b) -> a, Value.VBuiltin b)
  |> List.enum
  |> Dict.of_enum

  let init = {empty with v = init_v}

end
