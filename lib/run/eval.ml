open! Batteries
open Uref
open Either
open Enum

open Syntax

type _val = 
  | VInt of int
  | VStr of string
  | VQuote of callable
  | VList of callable

and callable = 
  | Closure of Ast.expr * Context.t
  | Thunk of fn * fn

and value = _val uref

and stack = value list
and costack = int * stack

and fn = costack -> costack Enum.t

type semantic = costack -> 
  (int * costack, (stack -> 
    (stack -> costack Enum.t) * stack
  ) * stack) Either.t

let rec meaning (ctx : Context.t) (e0 : Ast.expr) : semantic = 
  fun c -> match fst e0 with
  | Bop (e1, Tensor, e2) -> 
    begin match meaning ctx e1 c with
      | Left (i, c') -> Left (i, c')
      | Right (f1, s1) -> 
        let g1, z1 = f1 s1 in
        match meaning ctx e2 (0, z1) with
        | Left (i, c') -> Left (i, c')
        | Right (f2, s2) -> 
          let g2, z2 = f2 s2 in
          Right ((fun _ -> 
            (fun z -> Monad.bind (g2 z) @@ 
              function[@warning "-8"] 0, s -> g1 s), z2
          ), s2)
    end
    
  | Bop (e1, Ponder, e2) -> 
    begin match meaning ctx e1 c with
      | Right (f1, s1) -> Right (f1, s1)
      | Left (i, c') -> match meaning ctx e2 c' with
        | Right (f1, s1) -> Right (
          (fun s -> let f', s' = f1 s in (fun x -> 
            Enum.map (fun (i', y) -> i' + i, y) (f' x)), s'), s1)
        | Left (i', c'') -> Left (i' + i, c'')
    end

  | _ -> failwith "Todo!"

(* and meaning_op (_ctx : Context.t) (e0 : Ast.expr) : semantic = match fst e0 with
  | _ -> failwith "Todo" *)

and exec (m : semantic) : fn = fun c -> match m c with
  | Right (f1, s1) -> 
    let (f2, s2) = f1 s1 in
    f2 s2
  | Left (i, (ci, s)) -> singleton (i + ci, s)
