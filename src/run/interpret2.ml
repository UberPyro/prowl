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

  let encode_lst loc = List.fold_left begin fun a ex -> 
    Right (Pair ((a, loc), ex), loc)
  end (Left (Unit, loc))
  
  let encode_plst loc = List.fold_left begin fun a ex -> 
    PRight (PPair ((a, loc), ex), loc)
  end (PLeft (PUnit, loc))

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
    | StackComb c -> comb st c
    | Cap e1 -> e e1 st
    
    | Bop (e1, s, e2) -> infix e1 s e2 st
    | SectLeft (s, e2) -> sect_left s e2 st
    | SectRight (e1, s) -> sect_right e1 s st
    | Sect s -> sect s st

    | Cat lst -> List.fold_left (fun a x -> a >=> e x) pure lst st
    | Case elst -> List.fold_left begin fun a -> function
      | Gre, x -> a <|> e x
      | Rel, x -> e x <|> a
      | Cut, x -> e x |> alt_cut a
    end annihilate elst st
    | Inv lst -> List.fold_left (fun a x -> a *> e x) pure lst st
    
    | List elst -> e (encode_lst loc elst, loc) st
  
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
  
  and comb st = 
    List.fold_left begin fun m a -> m >>= fun st1 -> match a with
    | Dup i, _ -> List.at (S.s st1) (i-1) >: st1 |> pure
    | Zap i, _ -> restack (List.remove_at (i-1) (S.s st1)) st1 |> pure
    | Rot 2, _ ->
      let v2, v1, st2 = !:: st1 in
      (v1, v2) >:: st2 |> pure
    | Rot 3, _ -> 
      let v3, v2, st2 = !:: st1 in
      let v1, st3 = !: st2 in
      v1 >: ((v3, v2) >:: st3) |> pure
    | Rot _, _ -> failwith "Unimplemented - rot n"
    | Run i, _ -> 
        restack (List.remove_at (i-1) (S.s st1)) st1
        |> call (to_cap (List.at (S.s st1) (i-1)))
  end (pure st)

  and alt_rel f g = g <|> f

  and alt_cut f g = (f <|> g) >> cut

  and choose_alt = function
    | Gre -> (<|>)
    | Rel -> alt_rel
    | Cut -> alt_cut
  
  and choose_alt_flip gr x y = choose_alt gr y x

  and times_quant gr e1 qmin qmax st = match qmax - qmin with
  | qdiff when qmin < 0 || qdiff < 0 -> null
  | qdiff -> adv qmin e1 st >>= adv_alt gr qdiff e1

  and times_quant_while gr e1 qmin st = 
    if qmin < 0 then null
    else adv qmin e1 st >>= adv_alt_while gr e1
  
  and adv n e1 st = 
    List.fold_left (>=>) pure (List.make n (e e1)) st

  and adv_alt gr n e1 = 
    Enum.scanl (>=>) pure (Enum.repeat ~times:n (e e1))
    |> Enum.fold (choose_alt_flip gr) annihilate

  and adv_alt_while gr e1 st = 
    Enum.unfold pure begin fun b -> 
      let g = b >=> e e1 in
      if is_null (g st) then None
      else Some (g, g)
    end |> Enum.fold (choose_alt_flip gr) annihilate <| st

  and call c st =
    e (Capture.ast c) (st <-| c) <&> fun st' -> st' <-> st
  
  and p = failwith "Implement"

end
