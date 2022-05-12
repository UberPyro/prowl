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
  ctx: e Dict.t;
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
let bop st op = 
  match st.stk with
  | h2 :: h1 :: t -> Some {st with stk = op h1 h2 :: t}
  | _ -> failwith "Stack underflow"
let arith_bop st op = bop st begin function
    | VCapture (Int i1, _) -> begin function
        | VCapture (Int i2, _) -> VInt (op i1 i2)
        | _ -> failwith "Type Error: Expected int"
      end
    | v -> failwith ("Type Error: Expected int, value is " ^ show_e_val v)
  end

let rec program st (_, expr) = e st expr

and e st (expr, _) = match expr with
  | Int i -> lit st (VInt i)
  | Str s -> lit st (VStr s)
  | Bin (i1, v, i2) -> lit st (VBin (i1, v, i2))
  | Capture ast -> lit st (VCapture ast)

  | Sym "+" -> arith_bop st (+)
  | Sym "-" -> arith_bop st (-)
  | Sym "*" -> arith_bop st ( * )
  | Sym "/" -> arith_bop st (/)
  | Sym s -> e st (st.ctx --> s)

  (* TODO: Generalize, improve errors *)
  | Bop ((_, p1) as e1, "+", e2) -> 
    e st (Cat [Capture e1, p1; Capture e2, p1; Sym "+", p1], p1)
  | Bop ((_, p1) as e1, "*", e2) -> 
    e st (Cat [Capture e1, p1; Capture e2, p1; Sym "*", p1], p1)

  | Cat lst -> List.fold_left begin fun a x -> 
      Option.bind a (fun y -> e y x)
    end (Some st) lst
  
  | _ -> failwith "Unimplemented"
