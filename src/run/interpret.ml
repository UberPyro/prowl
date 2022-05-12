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
    | VInt i1 -> begin function
        | VInt i2 -> VInt (op i1 i2)
        | _ -> failwith "Type Error: Expected int"
      end
    | _ -> failwith "Type Error: Expected int"
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
  
  | _ -> failwith "Unimplemented"
