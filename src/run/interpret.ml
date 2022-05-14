open Batteries
module Dict = Map.Make(struct type t = string let compare = compare end)
open Dict.Infix
open LazyList.Infix

open Ast

let (>>=) x f = LazyList.(map f x |> concat)
let (>=>) f g x = x >>= f >>= g
let pure a = LazyList.(cons a nil)
let (<|>) f g x = (pure x >>= f)^@^(pure x >>= g)
let ( *> ) x c = x >>= fun _ -> c
let (<&>) x f = LazyList.map f x

let g ex st l r = st >>= ex l >>= ex r

(* type ty_val = 
  | YInt
  | YStr
  | YBin of ty_val list list
  | YCapture of ty_eff
         (* is class?     type ctx *)  (* data ctx *)
  | YSig of (bool * ty_val) Dict.t * ty_val Dict.t *)

type e_val = 
  | VInt of int
  | VStr of string
  | VPair of e * e
  | VLeft of e
  | VRight of e
  | VCapture of e
  | VImm of e
  | VUnit
                          (* is impl *)
  (* | VMod of ty_val Dict.t * (bool * ty_val) Dict.t *)
  [@@deriving show]

type st = {
  (* tyctx: ty_val Dict.t; *)
  ctx: e_val Dict.t;
  stk: e_val list;
}

let enhanced_show_e = function
  | Int i, _ -> string_of_int i
  | Str s, _ -> s
  | ex -> show_e ex

let string_of_v = function
  | VInt i -> string_of_int i
  | VStr s -> s
  | VUnit -> "<>"
  | VPair (e1, e2) -> 
    Printf.sprintf "(%s, %s)" (enhanced_show_e e1) (enhanced_show_e e2)
  | VLeft ex -> Printf.sprintf "(;%s)" (enhanced_show_e ex)
  | VRight ex -> Printf.sprintf "(%s;)" (enhanced_show_e ex)
  
  | _ -> failwith "Unimplemented - string_of_v"

let null_st = {(* tyctx=Dict.empty; *) ctx=Dict.empty; stk=[]}

let init_ctx = [

] |> List.enum |> Dict.of_enum

let encode_lst loc = List.fold_left begin fun a ex -> 
  Right (Pair ((a, loc), ex), loc)
end (Left (Unit, loc))

let lit st v = pure {st with stk = v :: st.stk}
let op st stk v = pure {st with stk = v :: stk}
let cap st stk ex = pure {st with stk = VImm ex :: stk}

let rec program st (_, expr) = e expr st

and e (expr, loc) st = match expr with
  | Int i -> lit st (VInt i)
  | Str s -> lit st (VStr s)
  | Unit -> lit st VUnit
  | Pair (e1, e2) -> lit st (VPair (e1, e2))
  | Left e1 -> lit st (VLeft e1)
  | Right e1 -> lit st (VRight e1)
  | Capture ast -> lit st (VCapture ast)
  | StackComb c -> comb st c
  | Cap e1 -> e e1 st

  | Bop (e1, "+", e2) -> arith_bop st e1 (+) e2
  | Bop (e1, "-", e2) -> arith_bop st e1 (-) e2
  | Bop (e1, "*", e2) -> arith_bop st e1 ( * ) e2
  | Bop (e1, "/", e2) -> arith_bop st e1 (/) e2
  | Bop (e1, "**", e2) -> arith_bop st e1 Int.pow e2

  | Bop (e1, "==", e2) -> cmp_bop st e1 (=) e2
  | Bop (e1, "/=", e2) -> cmp_bop st e1 (<>) e2
  | Bop (e1, ">", e2) -> cmp_bop st e1 (>) e2
  | Bop (e1, "<", e2) -> cmp_bop st e1 (<) e2
  | Bop (e1, ">=", e2) -> cmp_bop st e1 (>=) e2
  | Bop (e1, "<=", e2) -> cmp_bop st e1 (<=) e2

  | Bop (e1, "&", e2) -> (e e1 st) >>= e e2
  | Bop (e1, "|", e2) -> (e e1 <|> e e2) st
  | Bop (e1, "&&", e2) -> (e e1 st) *> (e e2 st)

  | Cat lst -> List.fold_left (fun a x -> a >>= (e x)) (pure st) lst
  | Case (e1, elst) -> 
    List.fold_left (fun a x -> a <|> e x) (e e1) (List.map snd elst) st
  
  | List elst -> e (encode_lst loc elst, loc) st
  
  | Id "to-int" -> begin match st.stk with
    | VStr h :: t -> op st t (VInt (int_of_string h))
    | _ -> failwith "Type Error: Expected string"
  end
  | Id s -> begin match st.ctx --> s with
    | VImm ex -> e ex st
    | x -> lit st x (* incomplete? *)
    | exception Not_found -> failwith ("Unbound id: " ^ s)

    (* | x -> 
      print_endline s;
      print_endline (show_e_val x); 
      failwith "Unimplemented - id" *)
  end
  | Let (lst, e1) -> List.fold_left begin fun a -> function
    | "", (PId s, _), ex -> a <-- (s, VImm ex)
    | "", (PCat ((PId s, z) :: t), y), ex ->
      a <-- (s, VImm (As ("", (PCat t, y), ex), z))
    | "", px, ex -> 
    begin match (e ex st) >>= (p px) |> LazyList.get with
      | None -> failwith "Failing Let expression"
      | Some (c, _) -> c.ctx
    end
    | _ -> failwith "Unimplemented - let op"
  end st.ctx lst |> fun ctx -> e e1 {st with ctx}

  | As ("", p1, e1) -> (p p1 st) >>= (e e1)

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
  | Quant (e1, Plus, Gre) -> 
    let rec loop lst st1 =
      let st2 = st1 >>= e e1 in
      match LazyList.get st2 with
      | Some _ -> loop (st2^@^lst) st2
      | None -> lst in
      loop (e e1 st) (pure st)
  
  | _ ->
    print_endline (show_e_t expr);
    failwith "Unimplemented - expression"

and arith_bop st0 e1 opx e2 = 
  e e1 st0 >>= e e2 >>= fun st -> match st.stk with
    | VInt i2 :: VInt i1 :: t -> op st t (VInt (opx i1 i2))
    | _ -> failwith "Type Error: Expected integer"

and cmp_bop st0 e1 op e2 = 
  e e1 st0 >>= e e2 >>= fun st -> match st.stk with
    | VInt i2 :: VInt i1 :: stk -> 
      if op i1 i2 then pure {st with stk} else LazyList.nil
    | _ -> failwith "Type Error: Expected integer"

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
    | VCapture ex -> e ex {stx with stk = List.remove_at (i-1) stx.stk}
    | _ -> failwith "Type Error: Cannot call nonclosure"
  end
  | Rot _, _ -> failwith "Unimplemented - rot n"
end (pure st0)

(* and upd_ctx st stk s vc = pure {stk; ctx = st.ctx <-- (s, VImm vc)} *)

and p (px, _) st = match px with
  | PId s -> begin match st.stk with
    | h :: t -> pure {stk = t; ctx = st.ctx <-- (s, h)}
    | _ -> failwith "Stack Underflow"
  end
  | PCat lst ->
    List.fold_left (fun a p1 -> a >>= (p p1)) (pure st) (List.rev lst)
  | PCapture (PId s, _) -> begin match st.stk with
    | VCapture vc :: t -> pure {stk = t; ctx = st.ctx <-- (s, VImm vc)}
    | _ -> failwith "Type Error: matching non-capture against capture (direct)"
  end
  | PCapture px -> begin match st.stk with
    | VCapture vc :: t -> e vc {st with stk = t} >>= p px
    | _ -> failwith "Type Error: matching non-capture against capture (indirect)"
  end
  | PPair ((PId s1, _), (PId s2, _)) -> begin match st.stk with
    | VPair (vc1, vc2) :: t -> 
      pure {stk = t; ctx = st.ctx <-- (s1, VImm vc1) <-- (s2, VImm vc2)}
    | _ -> failwith "Type Error: matching non-pair on pair (direct) (direct)"
  end
  | PPair (px1, (PId s2, _)) -> begin match st.stk with
    | VPair (vc1, vc2) :: t -> e vc1 {st with stk = t} >>= p px1 >>= begin 
      fun stx -> pure {stx with ctx = st.ctx <-- (s2, VImm vc2)}
    end
    | _ -> failwith "Type Error: matching non-pair on pair (indirect) (direct)"
  end
  | PPair ((PId s1, _), px2) -> begin match st.stk with
    | VPair (vc1, vc2) :: t -> 
      e vc2 {stk = t; ctx = st.ctx <-- (s1, VImm vc1)} >>= p px2
    | _ -> failwith "Type Error: matching non-pair on pair (direct) (indirect)"
  end
  | PPair (px1, px2)  -> begin match st.stk with
    | VPair (vc1, vc2) :: t -> 
      e vc1 {st with stk = t} >>= p px1 >>= e vc2 >>= p px2
    | _ -> failwith "Type Error: matching non-pair on pair (indirect) (indirect)"
  end
  | PLeft (PId s, _) -> begin match st.stk with
    | VLeft vc :: t -> pure {stk = t; ctx = st.ctx <-- (s, VImm vc)}
    | VRight _ :: _ -> LazyList.nil
    | _ -> failwith "Type Error: matching non-either on either (direct)"
  end
  | PLeft px -> begin match st.stk with
    | VLeft vc :: t -> e vc {st with stk = t} >>= p px
    | VRight _ :: _ -> LazyList.nil
    | _ -> failwith "Type Error: matching non-either on either (indirect)"
  end
  | PRight (PId s, _) -> begin match st.stk with
    | VRight vc :: t -> pure {stk = t; ctx = st.ctx <-- (s, VImm vc)}
    | VLeft _ :: _ -> LazyList.nil
    | _ -> failwith "Type Error: matching non-either on either (direct)"
  end
  | PRight px -> begin match st.stk with
    | VRight vc :: t -> e vc {st with stk = t} >>= p px
    | VLeft _ :: _ -> LazyList.nil
    | _ -> failwith "Type Error: matching non-either on either (indirect)"
  end
  | _ -> failwith "Unimplemented - pattern"
