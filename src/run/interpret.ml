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
  | Bop (e1, "**", e2) -> arith_bop st e1 Int.pow e2

  | Bop (e1, "&", e2) -> Option.bind (e st e1) (fun st2 -> e st2 e2)

  | Cat lst -> List.fold_left begin fun a x -> 
      Option.bind a (fun y -> e y x)
    end (Some st) lst
  
  | Id "to-int" -> begin match st.stk with
    | VStr h :: t -> Some {st with stk = VInt (int_of_string h) :: t}
    | _ -> failwith "Type Error: Expected string"
  end
  | Id s -> begin match st.ctx --> s with
    | VImm ex -> e st ex
    | x -> lit st x (* incomplete? *)

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
    begin match Option.bind (e st ex) (fun st2 -> p st2 px) with
      | None -> failwith "Failing Let expression"
      | Some c -> c.ctx
    end
    | _ -> failwith "Unimplemented - let op"
  end st.ctx lst |> fun ctx -> e {st with ctx} e1

  | As ("", p1, e1) -> Option.bind (p st p1) (fun st2 -> e st2 e1)

  | Quant (e1, Num e2, Gre) -> 
    Option.bind (e st e2) begin function
    | {stk = VInt i :: _; _} -> 
      if i < 0 then None
      else let rec loop a = function
        | 0 -> a
        | j -> loop (Option.bind a (fun stx -> e stx e1)) (j - 1)
      in loop (Some st) i
    | _ -> failwith "Bad arg in quantifier"
    end
  
  | _ -> failwith "Unimplemented - expression"

and arith_bop st0 e1 op e2 = 
  let st1 = e st0 e1 in
  let st2 = Option.bind st1 (fun st -> e st e2) in
  Option.bind st2 begin fun st -> match st.stk with
    | VInt i2 :: VInt i1 :: t -> 
      Some {st with stk = VInt (op i1 i2) :: t}
    | _ -> failwith "Type Error: Expected integer"
  end

and p (st : st) (px, _ : p) : st option = match px with
  | PId s -> begin match st.stk with
    | h :: t -> Some {stk = t; ctx = st.ctx <-- (s, h)}
    | _ -> failwith "Stack Underflow"
  end
  | PCat lst -> List.fold_left begin fun a p1 -> 
    Option.bind a (fun st -> p st p1)
  end (Some st) (List.rev lst)

  | _ -> failwith "Unimplemented - pattern"
