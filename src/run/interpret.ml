open Batteries
module Dict = Map.Make(struct type t = string let compare = compare end)
open Dict.Infix

open Ast

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
  
  | _ -> failwith "Unimplemented"

let null_st = {(* tyctx=Dict.empty; *) ctx=Dict.empty; stk=[]}

let init_ctx = [

] |> List.enum |> Dict.of_enum

let lit st v = Some {st with stk = v :: st.stk}

let rec program st (_, expr) = e st expr

and e st (expr, _) = match expr with
  | Int i -> lit st (VInt i)
  | Str s -> lit st (VStr s)
  | Bin (i1, v, i2) -> lit st (VBin (i1, v, i2))
  | Capture ast -> lit st (VCapture ast)

  | Bop (e1, "+", e2) -> arith_bop st e1 (+) e2
  | Bop (e1, "-", e2) -> arith_bop st e1 (-) e2
  | Bop (e1, "*", e2) -> arith_bop st e1 ( * ) e2
  | Bop (e1, "/", e2) -> arith_bop st e1 (/) e2

  | Bop (e1, "&", e2) -> Option.bind (e st e1) (fun st2 -> e st2 e2)

  | Cat lst -> List.fold_left begin fun a x -> 
      Option.bind a (fun y -> e y x)
    end (Some st) lst
  
  | Id "to-int" -> begin match st.stk with
    | VStr h :: t -> Some {st with stk = VInt (int_of_string h) :: t}
    | _ -> failwith "Type Error: Expected string"
  end
  | _ -> failwith "Unimplemented"

and arith_bop st0 e1 op e2 = 
  let st1 = e st0 e1 in
  let st2 = Option.bind st1 (fun st -> e st e2) in
  Option.bind st2 begin fun st -> match st.stk with
    | VInt i2 :: VInt i1 :: t -> 
      Some {st with stk = VInt (op i1 i2) :: t}
    | _ -> failwith "Type Error: Expected integer"
  end

and p st = function
  | PId s -> begin match st.stk with
    | h :: t -> Some {stk = t; ctx = st.ctx <-- (s, h)}
    | _ -> failwith "Stack Underflow"
  end
  | PCat lst -> List.rev lst |> List.fold_left begin fun a (p1, _) -> 
    Option.bind a (fun st -> p st p1)
  end (Some st)

  | _ -> failwith "Unimplemented"
