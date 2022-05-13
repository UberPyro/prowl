open Batteries
module Dict = Map.Make(struct type t = string let compare = compare end)
open Dict.Infix
open LazyList.Infix

open Ast

let (>>=) x f = LazyList.(map f x |> concat)
let (>=>) f g x = x >>= f >>= g
let pure a = LazyList.(cons a nil)
let (<|>) f g x = (pure x >>= f)^@^(pure x >>= g) 

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
  | VBin of int * e list * int
  | VCapture of e
  | VImm of e
                          (* is impl *)
  (* | VMod of ty_val Dict.t * (bool * ty_val) Dict.t *)
  [@@deriving show]

type st = {
  (* tyctx: ty_val Dict.t; *)
  ctx: e_val Dict.t;
  stk: e_val list;
}

let string_of_v = function
  | VInt i -> string_of_int i
  | VStr s -> s
  
  | _ -> failwith "Unimplemented - string_of_v"

let null_st = {(* tyctx=Dict.empty; *) ctx=Dict.empty; stk=[]}

let init_ctx = [

] |> List.enum |> Dict.of_enum

let lit st v = pure {st with stk = v :: st.stk}

let rec program st (_, expr) = e expr st

and e (expr, _) st = match expr with
  | Int i -> lit st (VInt i)
  | Str s -> lit st (VStr s)
  | Bin (i1, v, i2) -> lit st (VBin (i1, v, i2))
  | Capture ast -> lit st (VCapture ast)
  | StackComb c -> comb st c

  | Bop (e1, "+", e2) -> arith_bop st e1 (+) e2
  | Bop (e1, "-", e2) -> arith_bop st e1 (-) e2
  | Bop (e1, "*", e2) -> arith_bop st e1 ( * ) e2
  | Bop (e1, "/", e2) -> arith_bop st e1 (/) e2
  | Bop (e1, "**", e2) -> arith_bop st e1 Int.pow e2

  | Bop (e1, "&", e2) -> (e e1 st) >>= e e2
  | Bop (e1, "|", e2) -> pure st >>= (e e1 <|> e e2)

  | Cat lst -> List.fold_left (fun a x -> a >>= (e x)) (pure st) lst
  
  | Id "to-int" -> begin match st.stk with
    | VStr h :: t -> pure {st with stk = VInt (int_of_string h) :: t}
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
        | j -> loop (a >>= (e e1)) (j - 1)
      in loop (pure st) i
    | _ -> failwith "Bad arg in quantifier"
    end
  
  | _ ->
    print_endline (show_e_t expr);
    failwith "Unimplemented - expression"

and arith_bop st0 e1 op e2 = 
  let st1 = e e1 st0 in
  let st2 = st1 >>= (e e2) in
  st2 >>= begin fun st -> match st.stk with
    | VInt i2 :: VInt i1 :: t -> 
      pure {st with stk = VInt (op i1 i2) :: t}
    | _ -> failwith "Type Error: Expected integer"
  end

(* maybe abstract over the stack update *)
and comb st0 = List.fold_left begin fun st1 -> function
  | Dup i, _ -> 
    st1 >>= fun stx -> pure {stx with stk = List.at stx.stk (i-1) :: stx.stk}
  | Zap i, _ -> 
    st1 >>= fun stx -> pure {stx with stk = List.remove_at (i-1) stx.stk}
  | Rot 2, _ -> st1 >>= begin function 
    | ({stk = h1 :: h2 :: t; _} as stx) -> 
      pure {stx with stk = h2 :: h1 :: t}
    | _ -> failwith "Stack underflow (swap)"
  end
  | Run i, _ -> st1 >>= begin
    fun stx -> 
      match List.at stx.stk (i-1) with
      | VCapture ex -> e ex {stx with stk = List.remove_at (i-1) stx.stk}
      | _ -> failwith "Type Error: Cannot call nonclosure"
  end
  | Rot _, _ -> failwith "Unimplemented - rot n"
end (pure st0)

and p (px, _) st = match px with
  | PId s -> begin match st.stk with
    | h :: t -> pure {stk = t; ctx = st.ctx <-- (s, h)}
    | _ -> failwith "Stack Underflow"
  end
  | PCat lst -> List.fold_left begin fun a p1 -> 
    a >>= (p p1)
  end (pure st) (List.rev lst)

  | _ -> failwith "Unimplemented - pattern"
