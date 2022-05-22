(* The *Lazy* Prowl Interpreter *)

open Batteries
open Util

module D0 = Map.Make(struct type t = string let compare = compare end)
module Dict = struct
  include D0
  (* let pp fmt fmtr d = print (String.print) fmt fmtr d *)
  let pp fmt fmtr d = 
    enum d
    |> List.of_enum
    |> Format.pp_print_list Format.(
      fun f (a, b) -> pp_print_string f a; fmt f b
    ) fmtr
end
open Dict.Infix
open LazyList.Infix

open Ast

type ty_val = 
  | YInt
  | YStr
  | YPair of ty_val * ty_val
  | YEith of ty_val * ty_val
  | YUnit
  | YVoid
  | YWild
  [@@deriving show]

type e_val = 
  | VInt of int
  | VStr of string
  | VPair of v_imm * v_imm
  | VLeft of v_imm
  | VRight of v_imm
  | VImm of v_imm
  | VUnit
  | VMod of vmod
  | VImpl of e
  | VImplMod
  | VBuiltin of string
  [@@deriving show]

and v_imm = {
  capt : e;
  imm_ctx : e_val Dict.t;
  imm_impl_ctx : vmod list Dict.t
  (* imm_ty_ctx : ty_val Dict.t *)
}

and vmod = {
  spec_map: (string list * ty option) Dict.t; 
  def_map: e Dict.t;
  impl_map: e Dict.t;
  impl_ctx: vmod list Dict.t;
  ty_ctx: ty_val Dict.t;
  e_ctx: e_val Dict.t
}

type st = {
  ctx: e_val Dict.t;
  stk: e_val list;
  impl_ctx: vmod list Dict.t
} [@@deriving show]

let print_st {ctx; stk; impl_ctx} = 
  print_endline "---";
  print_endline "Stack: ";
  List.iter (show_e_val >> print_endline) stk;
  print_endline "\nContext: ";
  Dict.iter (fun s -> show_e_val >> Printf.printf "%s: %s\n" s) ctx;
  print_endline "\nImplicit Contexts:";
  Dict.iter (fun s _ -> print_endline s) impl_ctx;
  print_endline "---"

let (<&>) x f = LazyList.map f x
let (>>=) x f = x <&> f |> LazyList.concat
let (>=>) f g x = f x >>= g
let pure a = LazyList.(cons a nil)
let (<|>) f g x = f x ^@^ g x
let ( *> ) x c y = x y >>= fun _ -> c y

let g ex st l r = st >>= ex l >>= ex r

let enhanced_show_e = function
  | Int i, _ -> string_of_int i
  | Str s, _ -> s
  | ex -> show_e ex

let string_of_v = function
  | VInt i -> string_of_int i
  | VStr s -> s
  | VUnit -> "<>"
  | VPair ({capt=e1; _}, {capt=e2; _}) -> 
    Printf.sprintf "(%s, %s)" (enhanced_show_e e1) (enhanced_show_e e2)
  | VLeft {capt; _} -> Printf.sprintf "(;%s)" (enhanced_show_e capt)
  | VRight {capt; _} -> Printf.sprintf "(%s;)" (enhanced_show_e capt)
  
  | _ -> failwith "Unimplemented - string_of_v"

let null_st = {ctx=Dict.empty; stk=[]; impl_ctx=Dict.empty}

let null_vmod = {
  spec_map = Dict.empty;
  def_map = Dict.empty;
  impl_map = Dict.empty;
  impl_ctx = Dict.empty;
  ty_ctx = Dict.empty;
  e_ctx = Dict.empty;
}

let init_ctx = [
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
  "&&", "intersect"
]
|> List.map (fun (a, b) -> a, VBuiltin b)
|> List.enum
|> Dict.of_enum

let init_st = {ctx=init_ctx; stk=[]; impl_ctx=Dict.empty}

let encode_lst loc = List.fold_left begin fun a ex -> 
  Right (Pair ((a, loc), ex), loc)
end (Left (Unit, loc))

let encode_plst loc = List.fold_left begin fun a ex -> 
  PRight (PPair ((a, loc), ex), loc)
end (PLeft (PUnit, loc))

let lit st v = pure {st with stk = v :: st.stk}
let op st stk v = pure {st with stk = v :: stk}

let rec program st (_, expr) = e expr st

and e (expr, loc) st = match expr with
  | Int i -> lit st (VInt i)
  | Str s -> lit st (VStr s)
  | Unit -> lit st VUnit
  | Pair (e1, e2) -> lit st (VPair (
    {capt=e1; imm_ctx=st.ctx; imm_impl_ctx=st.impl_ctx},
    {capt=e2; imm_ctx=st.ctx; imm_impl_ctx=st.impl_ctx}
  ))
  | Left e1 ->
    lit st (VLeft {capt=e1; imm_ctx=st.ctx; imm_impl_ctx=st.impl_ctx})
  | Right e2 -> 
    lit st (VRight {capt=e2; imm_ctx=st.ctx; imm_impl_ctx=st.impl_ctx})
  | Capture ast ->
    lit st (VImm {capt=ast; imm_ctx=st.ctx; imm_impl_ctx=st.impl_ctx})
  | StackComb c -> comb st c
  | Cap e1 -> e e1 st

  | Bop (e1, s, e2) -> infix st e1 s e2
  | SectLeft (s, e2) -> sect_left st s e2
  | SectRight (e1, s) -> sect_right st e1 s
  | Sect s -> sect st s

  | Cat lst -> List.fold_left (fun a x -> a >>= (e x)) (pure st) lst
  | Case (e1, elst) -> 
    List.fold_left (fun a x -> a <|> e x) (e e1) (List.map snd elst) st
  
  | List elst -> e (encode_lst loc elst, loc) st
  
  | Id "to-int" -> begin match st.stk with
    | VStr h :: t -> op st t (VInt (int_of_string h))
    | _ -> failwith "Type Error: Expected string"
  end
  | Id s -> begin match st.ctx --> s with
    | VBuiltin "add" -> arith_builtin st (+)
    | VBuiltin "sub" -> arith_builtin st (-)
    | VBuiltin "mul" -> arith_builtin st ( * )
    | VBuiltin "div" -> arith_builtin st (/)
    | VBuiltin "exp" -> arith_builtin st Int.pow

    | VBuiltin "eq" -> cmp_builtin st (==)
    | VBuiltin "ne" -> cmp_builtin st (<>)
    | VBuiltin "gt" -> cmp_builtin st (>)
    | VBuiltin "lt" -> cmp_builtin st (<)
    | VBuiltin "ge" -> cmp_builtin st (>=)
    | VBuiltin "le" -> cmp_builtin st (<=)

    | VBuiltin "cat" -> combinator st (>=>)
    | VBuiltin "alt" -> combinator st (<|>)
    | VBuiltin "intersect" -> combinator st ( *> )

    | VImm {capt=ex; imm_ctx=ctx; imm_impl_ctx=impl_ctx} -> 
      e ex {st with ctx; impl_ctx} <&> fun stx ->
      {stx with ctx = st.ctx; impl_ctx = st.impl_ctx}
    | x -> lit st x (* incomplete? *)
    | exception Not_found ->
      print_st st;
      failwith
        (Printf.sprintf "Unbound id: %s @ [%d:%d]"
        s
        (fst loc).pos_lnum ((fst loc).pos_cnum - (fst loc).pos_bol))

    (* | x -> 
      print_endline s;
      print_endline (show_e_val x); 
      failwith "Unimplemented - id" *)
  end
  | Let (lst, e1) -> List.fold_left begin fun a -> function
    | "", false, (PId s, _), ex -> a <&> begin 
        fun stx -> {stx with ctx = stx.ctx <-- (s, VImm {
          capt = ex;
          imm_ctx = stx.ctx;
          imm_impl_ctx = stx.impl_ctx
        })}
      end
    | "", false, (PCat ((PId s, z) :: t), y), ex -> a <&> begin
        fun stx -> {stx with 
          ctx = stx.ctx <-- (s, VImm {
            capt = As ("", (PCat t, y), ex), z;
            imm_ctx = stx.ctx;
            imm_impl_ctx = stx.impl_ctx
        })
        }
      end
    | "", false, px, ex -> a >>= e ex >>= p px
    (* Note: broken *)
    | a, _, px, ex -> e (Id ("let" ^ a), loc) {
      st with stk = VImm {
        capt = As ("", px, e1), loc;
        imm_ctx = st.ctx; 
        imm_impl_ctx = st.impl_ctx
      } :: VImm {
        capt = ex; 
        imm_ctx = st.ctx; 
        imm_impl_ctx = st.impl_ctx
      } :: st.stk
    }
  end (pure st) lst >>= e e1 <&> fun stx -> {st with stk = stx.stk}

  | As ("", p1, e1) -> 
    (p p1 st) >>= e e1 <&> fun stx -> {st with stk = stx.stk}
  | As (s, p1, e1) -> e (Id ("as" ^ s), loc) {
    st with stk = VImm {
      capt = As ("", p1, e1), loc;
      imm_ctx = st.ctx;
      imm_impl_ctx = st.impl_ctx
    } :: st.stk
  }

  | Quant (e1, Num e2, Gre) -> 
    (e e2 st) >>= begin function
    | {stk = VInt i :: _; _} -> 
      if i < 0 then LazyList.nil
      else let rec loop a = function
        | 0 -> a
        | j -> loop (a >>= e e1) (j - 1)
      in loop (pure st) i
    | _ -> failwith "Bad arg in quantifier"
    end
  
  | Quant (e1, Star, Gre) -> 
    let rec loop lst st1 =
      let st2 = st1 >>= e e1 in
      match LazyList.get st2 with
      | Some _ -> loop (st2^@^lst) st2
      | None -> lst in
      loop (pure st) (pure st)
    
  | Quant (e1, Star, Cut) -> 
    let rec loop stf st1 =
      let st2 = st1 >>= e e1 in
      match LazyList.get st2 with
      | Some _ -> loop (st2) st2
      | None -> stf in
      loop (pure st) (pure st)
    
  | Quant (e1, Plus, Gre) -> 
    let rec loop lst st1 =
      let st2 = st1 >>= e e1 in
      match LazyList.get st2 with
      | Some _ -> loop (st2^@^lst) st2
      | None -> lst in
      loop (e e1 st) (pure st)
    
  | Quant (e1, Plus, Cut) -> 
    let rec loop stf st1 =
      let st2 = st1 >>= e e1 in
      match LazyList.get st2 with
      | Some _ -> loop (st2) st2
      | None -> stf in
      loop (e e1 st) (pure st)
    
  | Quant (e1, Opt, Gre) -> (e e1 <|> pure) st
  | Quant (e1, Opt, Cut) -> 
    let st1 = e e1 st in
    begin match LazyList.get st1 with
      | None -> pure st
      | _ -> st1
    end

  | Mod lst -> List.fold_left begin fun a -> function
    | Def (access, false, (PId s, _), e1, _), _ -> {
      a with
      def_map = begin match access with 
        | Pub -> Dict.add s e1 a.def_map
        | Local -> a.def_map
        | Opaq -> failwith "Values cannot be opaque"
      end;
      e_ctx = Dict.add s (
        VImm {capt=e1; imm_ctx=a.e_ctx; imm_impl_ctx=a.impl_ctx}
      ) a.e_ctx
    }
    | Def (access, false, (PCat ((PId s, z) :: t), y), e1, _), _ -> 
      let e2 = As ("", (PCat t, y), e1), z in {
      a with
      def_map = begin match access with 
        | Pub -> Dict.add s e2 a.def_map
        | Local -> a.def_map
        | Opaq -> failwith "Values cannot be opaque"
      end; 
      e_ctx = Dict.add s (
        VImm {capt=e1; imm_ctx=a.e_ctx; imm_impl_ctx=a.impl_ctx}
      ) a.e_ctx
    }
    | Def (access, true, (PId s, _), e1, _), _ -> 
      begin match LazyList.get (e e1 st) with
        | Some ({stk = VMod vmod :: _; _}, _) -> {
          a with
          impl_map = begin match access with
            | Pub -> Dict.add s e1 a.def_map
            | Local -> a.def_map
            | Opaq -> failwith "Values cannot be opaque"
          end;
          impl_ctx = Dict.fold begin fun k _ b -> 
            b <-- (k, vmod :: try b --> k with Not_found -> [])
          end vmod.def_map a.impl_ctx
        }
      | _ -> failwith "bad"
      end
    | Ty (access, s, args, ty1), _ -> {
      a with
      spec_map = begin match access with
        | Pub -> Dict.add s (args, ty1) a.spec_map
        | Local -> a.spec_map
        | Opaq -> Dict.add s (args, None) a.spec_map
      end
    }
    | Open (_, e1), _ ->
      e e1 {st with ctx = a.e_ctx} 
      |> LazyList.hd |> begin fun stx -> match stx.stk with
      | VMod {def_map; _} :: _ -> def_map
      | _ -> failwith "Type Error: Cannot open non-module"
    end |> fun e_map -> {
      a with e_ctx =
        Dict.map begin 
          fun x -> VImm {capt=x; imm_ctx=a.e_ctx; imm_impl_ctx=a.impl_ctx}
        end e_map |> Dict.union (fun _ _ z -> Some z) a.e_ctx
    }
    | Mix e1, _ ->
      e e1 {st with ctx = a.e_ctx} 
      |> LazyList.hd |> begin fun stx -> match stx.stk with
      | VMod {def_map; _} :: _ -> def_map
      | _ -> failwith "Type Error: Cannot mix non-module"
    end |> fun def_map -> {a with def_map}
    
    | _ -> a (* temporary *)
  end {null_vmod with e_ctx = st.ctx; impl_ctx = st.impl_ctx} lst
  |> fun vmod -> lit st (VMod vmod)

  | Access (e1, s) -> e e1 st >>= fun stx -> 
    begin match stx.stk with
      | VMod vmod :: _ ->
        begin try e (vmod.def_map --> s) {st with ctx = vmod.e_ctx} with
        | Not_found -> failwith "Field not found in module" end
      | VImplMod :: _ ->
        begin try st.impl_ctx --> s with
          | Not_found -> failwith "Field not found in implicit context"
        end |> List.filter begin fun vmod -> (* vmod.def_map is empty? *)
        match vmod.def_map --> s with
        | As (_, (PAsc (_, t1), _), _), _ ->
          ty_of_v (List.hd st.stk) == y_of_ty t1
        | As (_, (PCat lst, _), _), _ -> 
          List.take (List.length lst) st.stk
          |> List.combine lst
          |> List.map begin function 
            | ((PAsc (_, t1), _), v) -> ty_of_v v == y_of_ty t1
            (* | p1, _ -> show_p p1 |> print_endline; failwith "end" *)
            | _ -> failwith "Bad implicits matching - missing an annotation?"
          end |> List.for_all identity
        | exception Not_found -> false  (* name not in module *)
        | _ -> failwith "Bad Implicits case"
      end |> begin function
        | [] -> failwith "Implicits Error: No match could be found"
        | _ :: _ :: _ -> failwith "Implicits Error: More than 1 match"
        | [vmod] ->
          try e (vmod.def_map --> s) {st with ctx = vmod.e_ctx} with
          | Not_found -> failwith "Internal Error: Selected module does not have field"
      end
      | _ -> failwith "Type Error: Accessing a non-module"
    end <&> fun stx -> {stx with ctx = st.ctx}
  | Impl e1 -> lit st (VImpl e1)
  
  | _ ->
    print_endline (show_e_t expr);
    failwith "Unimplemented - expression"

and arith_builtin st opx = match st.stk with
  | VImm {capt=e1; imm_ctx=ctx1; _} :: VImm {capt=e2; imm_ctx=ctx2; _} :: t ->
    e e2 {st with stk=t; ctx=Dict.union (fun _ _ c -> Some c) st.ctx ctx2} <&> begin
      fun sty -> {sty with ctx=Dict.union (fun _ _ c -> Some c) st.ctx ctx1}
    end >>= e e1 <&> fun stx -> begin match stx.stk with
    | VInt i1 :: VInt i2 :: t -> {stx with stk = VInt (opx i1 i2) :: t}
    | _ -> failwith "Type Error: Expected Integer"
    end
  | _ -> failwith "Stack Underflow - Arithmetic Builtin"

and cmp_builtin st opx = match st.stk with
  | VImm {capt=e1; imm_ctx=ctx1; _} :: VImm {capt=e2; imm_ctx=ctx2; _} :: t ->
    e e2 {st with stk=t; ctx=Dict.union (fun _ _ c -> Some c) st.ctx ctx2} <&> begin
      fun sty -> {sty with ctx=Dict.union (fun _ _ c -> Some c) st.ctx ctx1}
    end >>= e e1 >>= fun stx -> begin match stx.stk with
    | VInt i1 :: VInt i2 :: t -> 
      if opx i1 i2 then pure {stx with stk=t} else LazyList.nil
    | _ -> failwith "Type Error: Expected Integer"
    end
  | _ -> failwith "Stack Underflow - Comparison Builtin"

and combinator st opx = match st.stk with
  | VImm h1 :: VImm h2 :: stk ->
    opx (e h1.capt) (e h2.capt) {st with stk}
  | _ -> failwith "Stack Underflow - Combinator"

and infix st (_, loc as e1) opx e2 = e (Id opx, loc) {
  st with
  stk = VImm {capt=e1; imm_ctx = st.ctx; imm_impl_ctx=st.impl_ctx}
  :: VImm {capt=e2; imm_ctx = st.ctx; imm_impl_ctx=st.impl_ctx}
  :: st.stk
}

and sect_left st opx (_, loc as e2) = match st.stk with
  | [] -> failwith (Printf.sprintf "Stack Underflow - Left Section: (%s _)" opx)
  | h1 :: t -> e (Id opx, loc) {
    st with stk = VImm {
      capt=Id "@1", loc;
      imm_ctx=Dict.add "@1" h1 st.ctx;
      imm_impl_ctx=st.impl_ctx
    }
    :: VImm {capt=e2; imm_ctx = st.ctx; imm_impl_ctx=st.impl_ctx}
    :: t
  }

and sect_right st (_, loc as e1) opx = match st.stk with
  | [] -> failwith (Printf.sprintf "Stack Underflow - Right Section: (_ %s)" opx)
  | h2 :: t -> e (Id opx, loc) {
    st with 
    stk = VImm {capt=e1; imm_ctx = st.ctx; imm_impl_ctx=st.impl_ctx}
    :: VImm {
      capt=Id "@2", loc;
      imm_ctx=Dict.add "@2" h2 st.ctx;
      imm_impl_ctx=st.impl_ctx
    }
    :: t
  }

and sect st opx = match st.stk with
  | _ :: [] | [] -> 
    failwith (Printf.sprintf "Stack Underflow - Section (%s)" opx)
  | h2 :: h1 :: t -> e (Id opx, dum) {
    st with
    stk = VImm {
      capt=Id "@1", dum;
      imm_ctx=Dict.add "@1" h1 st.ctx;
      imm_impl_ctx=st.impl_ctx
    }
    :: VImm {
      capt=Id "@2", dum;
      imm_ctx=Dict.add "@2" h2 st.ctx;
      imm_impl_ctx=st.impl_ctx
    }
    :: t
  }

and bop_rewrite st (_, loc as e1) opx e2 =
  e e1 st >>= e e2 >>= e (Id opx, loc)

(* maybe abstract over the stack update *)
and comb st0 = List.fold_left begin fun st1 -> function
  | Dup i, _ -> st1 >>= fun stx -> op stx stx.stk (List.at stx.stk (i-1))
  | Zap i, _ -> st1 <&> fun stx -> 
    {stx with stk = List.remove_at (i-1) stx.stk}
  | Rot 2, _ -> st1 >>= begin function 
    | ({stk = h1 :: h2 :: t; _} as stx) -> 
      pure {stx with stk = h2 :: h1 :: t}
    | _ -> failwith "Stack underflow (swap)"
  end
  | Rot 3, _ -> st1 >>= begin function 
    | ({stk = h1 :: h2 :: h3 :: t; _} as stx) -> 
      pure {stx with stk = h3 :: h2 :: h1 :: t}
    | _ -> failwith "Stack underflow (rot)"
  end
  | Run i, _ -> st1 >>= begin fun stx -> 
    match List.at stx.stk (i-1) with
    | VImm {capt; imm_ctx; imm_impl_ctx} -> e capt {
      stk = List.remove_at (i-1) stx.stk;
      ctx = imm_ctx;
      impl_ctx = imm_impl_ctx
    } <&> fun sty -> {sty with ctx=stx.ctx; impl_ctx=stx.impl_ctx}
    | _ -> failwith "Type Error: Cannot call nonclosure"
  end
  | Rot _, _ -> failwith "Unimplemented - rot n"
end (pure st0)

(* and upd_ctx st stk s vc = pure {stk; ctx = st.ctx <-- (s, VImm vc)} *)

and p (px, loc) st = match px with
  | PId s -> begin match st.stk with
    | h :: t -> pure {st with stk = t; ctx = st.ctx <-- (s, h)}
    | _ -> failwith "Stack Underflow"
  end
  | PCat lst ->
    List.fold_left (fun a p1 -> a >>= (p p1)) (pure st) (List.rev lst)
  | PCapture (PId s, _) -> begin match st.stk with
    | (VImm _ as vi) :: t -> pure {
      st with
      stk = t; 
      ctx = st.ctx <-- (s, vi)
    }
    | _ -> failwith "Type Error: matching non-capture against capture (direct)"
  end
  | PCapture px -> begin match st.stk with
    | VImm {capt; _} :: t -> e capt {st with stk = t} >>= p px
    | _ -> failwith "Type Error: matching non-capture against capture (indirect)"
  end
  | PAsc (p1, _) -> p p1 st
  | PPair ((PId s1, _), (PId s2, _)) -> begin match st.stk with
    | VPair (vc1, vc2) :: t -> pure {
        st with
        stk = t;
        ctx = st.ctx <-- (s1, VImm vc1) <-- (s2, VImm vc2)
      }
    | _ -> failwith "Type Error: matching non-pair on pair (direct) (direct)"
  end
  | PPair (px1, (PId s2, _)) -> begin match st.stk with
    | VPair ({capt=vc1; _}, vc2) :: t ->
      e vc1 {st with stk = t} >>= p px1 >>= begin fun stx -> pure {
        stx with ctx = st.ctx <-- (s2, VImm vc2)
      }
    end
    | _ -> failwith "Type Error: matching non-pair on pair (indirect) (direct)"
  end
  | PPair ((PId s1, _), px2) -> begin match st.stk with
    | VPair (vc1, {capt=vc2; _}) :: t -> e vc2 {
        st with
        stk = t;
        ctx = st.ctx <-- (s1, VImm vc1)
      } >>= p px2
    | _ -> failwith "Type Error: matching non-pair on pair (direct) (indirect)"
  end
  | PPair (px1, px2)  -> begin match st.stk with
    | VPair ({capt=vc1; _}, {capt=vc2; _}) :: t -> 
      e vc1 {st with stk = t} >>= p px1 >>= e vc2 >>= p px2
    | _ -> failwith "Type Error: matching non-pair on pair (indirect) (indirect)"
  end
  | PLeft (PId s, _) -> begin match st.stk with
    | VLeft vc :: t -> pure {
        st with
        stk = t;
        ctx = st.ctx <-- (s, VImm vc)
      }
    | VRight _ :: _ -> LazyList.nil
    | _ -> failwith "Type Error: matching non-either on either (direct)"
  end
  | PLeft px -> begin match st.stk with
    | VLeft {capt=vc; _} :: t -> e vc {st with stk = t} >>= p px
    | VRight _ :: _ -> LazyList.nil
    | _ -> failwith "Type Error: matching non-either on either (indirect)"
  end
  | PRight (PId s, _) -> begin match st.stk with
    | VRight vc :: t -> pure {
        st with
        stk = t;
        ctx = st.ctx <-- (s, VImm vc)
      }
    | VLeft _ :: _ -> LazyList.nil
    | _ -> failwith "Type Error: matching non-either on either (direct)"
  end
  | PRight px -> begin match st.stk with
    | VRight {capt=vc; _} :: t -> e vc {st with stk = t} >>= p px
    | VLeft _ :: _ -> LazyList.nil
    | _ -> failwith "Type Error: matching non-either on either (indirect)"
  end
  | PImpl (PId s, _ as p1, _) -> begin match st.stk with
    | VImpl e1 :: t -> e e1 {st with stk = t} >>= p p1
    | _ -> pure {st with ctx = st.ctx <-- (s, VImplMod)}
  end
  | POpen false -> begin match st.stk with
    | VMod {def_map; _} :: t -> def_map, t
    | _ -> 
      print_st st;
      failwith "Type Error: matching non-module against OPEN"
  end |> fun (e_map, t) -> pure {
    st with stk = t; ctx = 
      Dict.map (fun x -> VImm {
        capt=x;
        imm_ctx=st.ctx;
        imm_impl_ctx=st.impl_ctx
      }) e_map |> Dict.union (fun _ _ z -> Some z) st.ctx
  }
  | PBop (p1, ">-", p2) -> p (PRight (PPair (p1, p2), loc), loc) st
  | PList [] -> p (PLeft (PUnit, loc), loc) st
  (* FIXME: then delete above since it's redundant *)
  | PList plst -> p (encode_plst loc plst, loc) st
  | PUnit -> begin match st.stk with
    | VUnit :: stk -> pure {st with stk}
    | _ -> failwith "Type Error: matching non-unit against UNIT"
  end

  | pz ->
    show_p_t pz |> print_endline;
    failwith "Unimplemented - pattern"

and ty_of_v = function
  | VInt _ -> YInt
  | VStr _ -> YStr
  | VPair ({capt=e1; _}, {capt=e2; _}) -> YPair (ty_of_e e1, ty_of_e e2)
  | VLeft {capt=e1; _} -> YEith (ty_of_e e1, YWild)
  | VRight {capt=e2; _} -> YEith (YWild, ty_of_e e2)
  | VImm _ -> failwith "Cannot deduce the type of a function"
  | VUnit -> YUnit
  | VMod _ -> failwith "Cannot deduce the type of a module"
  | _ -> failwith "Cannot convert value to type"

and ty_of_e e1 = match e e1 null_st |> LazyList.get with
  | None -> failwith "Getting the type of a rejection"
  | Some ({stk = h :: _; _}, _) -> ty_of_v h
  | Some (_, _) -> failwith "Getting the type of a non-pushing expression"

and y_of_ty x =
  begin fst >> snd >> fst >> function
    | TId "int" -> YInt
    | TId "str" -> YStr
    | TBin bin -> 
      List.map (fun a -> List.map y_of_ty a) bin
      |> List.map (bin_group (fun a b -> YPair (a, b)))
      |> bin_group (fun a b -> YEith (a, b))
    (* | TList (_, loc) as t -> 
      y_of_ty (TBin [[((TCat [], loc), (TUnit, loc)), loc]; []]) *)
    | ty1 -> 
      show_ty_term_t ty1 |> print_endline;
      failwith "Unimplemented"
  end x

and bin_group g = function
  | [h1; h2] -> g h1 h2
  | h1 :: h2 :: t -> bin_group g (g h1 h2 :: t)
  | lst ->
    List.length lst
    |> Printf.sprintf "Cannot have an either of %d elem(s)"
    |> failwith
