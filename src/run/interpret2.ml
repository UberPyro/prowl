open Batteries
open Util

open State

module V = Value
module T = Type
module M = Module
module C = Capture
module S = State

module Run (E : Eval.S) = struct

  open Ast
  open V
  open S
  open S.Infix
  open E

  let lit v st = pure (push v st)

  let rec program (_, e1) = e e1

  and e (e0, loc) st = match e0 with
    | Int i -> lit (VInt i) st
    | Str s -> lit (VStr s) st
    | Unit -> lit VUnit st
    | Pair (e1, e2) -> lit (VPair (C.of_st e1 st, C.of_st e2 st)) st
    | Left e1 -> lit (VLeft (C.of_st e1 st)) st
    | Right e2 -> lit (VRight (C.of_st e2 st)) st
    | Capture ast -> lit (VImm (C.of_st ast st)) st
    (* | StackComb c -> comb st c *)
    | Cap e1 -> e e1 st
  
    | _ -> failwith "Unimplemented - expression"
  
  and arith_builtin o st = 
    let v2, v1, st' = !:: st in
    let c2, c1 = V.(to_cap v2, to_cap v1) in
    call c2 st' >>= call c1 <&> fun st2 -> 
      let v1, v2, st3 = !:: st2 in
      let v3 = VInt (o (V.to_int v1) (V.to_int v2)) in
      v3 >: st3
  
  and cmp_builtin o st = 
    let v2, v1, st' = !:: st in
    let c2, c1 = V.(to_cap v2, to_cap v1) in
    call c2 st' >>= call c1 >>= fun st2 -> 
      let v1, v2, st3 = !:: st2 in
      if o (V.to_int v1) (V.to_int v2) then pure st3
      else null

  and combinator o st = 
    let v2, v1, st' = !:: st in
    let c1, c2 = V.(to_cap v2, to_cap v1) in
    o (call c1) (call c2) st'
  
  and infix (_, loc as e1) o e2 st = 
    e (Id o, loc) (C.(VImm (of_st e2 st), VImm (of_st e1 st)) >:: st)
  
  and def_cap s v st = C.of_st (Id s, dum) (st <-- (s, v))
  
  and sect_left o (_, loc as e2) st = 
    let v2, st' = !: st in
    e (Id o, loc) (C.(VImm (of_st e2 st), VImm (def_cap "@2" v2 st)) >:: st')
  
  and sect_right (_, loc as e1) o st = 
    let v1, st' = !: st in
    e (Id o, loc) (C.(VImm (def_cap "@1" v1 st), VImm (of_st e1 st)) >:: st')
  
  and sect o st = 
    let v2, v1, st' = !:: st in
    e (Id o, dum) begin (
      VImm (def_cap "@2" v2 st), 
      VImm (def_cap "@1" v1 st)
    ) >:: st'
  end
  
  and comb st = List.fold_left begin fun st1 -> function
    | Dup i, _ -> st1 <&> fun st2 -> List.at (S.s st2) (i-1) >: st2
    (* | Zap i, _ ->  *)
  end

  and call c st =
    e (Capture.ast c) (st <-| c) <&> fun st' -> st' <-> st

end
