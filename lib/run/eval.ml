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
          Right ((fun zx -> 
            let g1, z1 = f1 zx in
            match meaning ctx e2 (0, z1) with
            | Left _ -> failwith "second tensor argument must be branchless"
            | Right (f2, s2) -> 
              let g2, z2 = f2 s2 in
              (fun z -> Monad.bind (g2 z) @@ 
                function[@warning "-8"] 0, s -> g1 s), z2
          ), s1)
      end
  | Bop (e1, Fork, e2) -> 
    begin match meaning ctx e1 c with
    | Left (i, c') -> Left (i, c')
    | Right (f1, s1) -> 
      Right ((fun zx -> 
        let g1, z1 = f1 zx in
        match meaning ctx e2 (0, zx) with
        | Left _ -> failwith "second fork argument must be branchless"
        | Right (f2, s2) -> 
          let g2, z2 = f2 s2 in
          if z1 == z2 then (fun z -> Monad.bind (g2 z) @@ 
            function[@warning "-8"] 0, s -> g1 s), z2
          else failwith "fork arguments must have same in-arity"
          (* else match stack_sub_sym z1 z2 with
            | Left h -> (fun z -> Monad.bind (g2 (h @ z)) @@
              function[@warning "-8"] 0, s -> g1 s), z1
            | Right _ -> (fun z -> Monad.bind (g2 z) @@ 
              function[@warning "-8"] 0, s -> g1 s), z1 *)
      ), s1)
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
  (* problem -- need to know # of arguments eliminated from costack *)
  | Bop (e1, Pick, e2) -> 
    begin match meaning ctx e1 c with
      | Right (f1, s1) -> Right (f1, s1)
      | Left (i1, c1) -> match meaning ctx e2 c1 with
        | Right (f2, s2) -> Right (f2, s2)
        | Left (i2, c2) -> 
          if i1 = i2 then Left (i2, c2)
          else failwith "pick arguments must have same out-arity"
        
    end

  | _ -> failwith "Todo!"

(* and meaning_op (_ctx : Context.t) (e0 : Ast.expr) : semantic = match fst e0 with
  | _ -> failwith "Todo" *)

and exec (m : semantic) : fn = fun c -> match m c with
  | Right (f1, s1) -> 
    let (f2, s2) = f1 s1 in
    f2 s2
  | Left (i, (ci, s)) -> singleton (i + ci, s)

and stack_sub_sym z1 z2 = 
  if List.(length z1 < length z2) then Left (stack_subtract z2 z1)
  else Right (stack_subtract z1 z2)

and stack_subtract z_larger z = 
  if z_larger == z then []
  else match z with
    | h :: t -> h :: stack_subtract z_larger t
    | [] -> failwith "fork"
