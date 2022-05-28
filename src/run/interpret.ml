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

    | Id "to-int" -> 
      let v, st1 = !: st in
      VInt (V.to_int v) >: st1 |> pure

    | Id s -> begin match st --> s with
      | VBuiltin "add" -> arith_builtin (+) st
      | VBuiltin "sub" -> arith_builtin (-) st
      | VBuiltin "mul" -> arith_builtin ( * ) st
      | VBuiltin "div" -> arith_builtin (/) st
      | VBuiltin "exp" -> arith_builtin Int.pow st

      | VBuiltin "eq" -> cmp_builtin (=) st
      | VBuiltin "ne" -> cmp_builtin (<>) st
      | VBuiltin "gt" -> cmp_builtin (>) st
      | VBuiltin "lt" -> cmp_builtin (<) st
      | VBuiltin "ge" -> cmp_builtin (>=) st
      | VBuiltin "le" -> cmp_builtin (<=) st

      | VBuiltin "cat" -> combinator (>=>) st
      | VBuiltin "alt" -> combinator (<|>) st
      | VBuiltin "alt-rel" -> combinator alt_rel st
      | VBuiltin "alt-cut" -> combinator alt_cut st
      | VBuiltin "intersect" -> combinator ( *> ) st

      | VImm c -> call c st
      | x -> lit x st
      | exception Not_found -> 
          failwith
            (Printf.sprintf "Unbound id: %s @ [%d:%d]"
            s
            (fst loc).pos_lnum ((fst loc).pos_cnum - (fst loc).pos_bol))
    end
    | Let (lst, e1) -> List.fold_left begin fun a -> function
      | "", false, (PId s, _), ex -> a <&> fun stx -> 
        set s (VImm (C.of_st ex stx)) stx
      | "", false, (PCat ((PId s, z) :: t), y), ex -> a <&> fun stx -> 
          set s (VImm (C.of_st (As ("", (PCat t, y), ex), z) stx)) stx
      | "", false, px, ex -> a >>= e ex >>= p px
      (* Note: broken *)
      | b, _, px, ex -> a >>= fun st' -> 
        let l = VImm (C.of_st (As ("", px, e1), loc) st') in
        let r = VImm (C.of_st ex st') in
        e (Id ("let" ^ b), loc) ((l, r) >:: st')
    end (pure st) lst >>= e e1 <&> fun stx -> stx <-> st

    | As ("", p1, e1) -> (p p1 st) >>= e e1 <&> fun st1 -> st1 <-> st
    | As (s, p1, e1) -> 
      e (Id ("as" ^ s), loc) (VImm (C.of_st (As ("", p1, e1), loc) st) >: st)
    
    | Quant (e1, Num e2, gr) -> 
      eval_grab_int e2 st (fun i1 -> times_quant gr e1 i1 i1)
    
    | Quant (e1, Min e2, gr) -> 
      eval_grab_int e2 st (fun i1 st1 -> times_quant_while gr e1 i1 st1)

    | Quant (e1, Max e2, gr) -> 
      eval_grab_int e2 st (fun i1 st1 -> times_quant gr e1 0 i1 st1)

    | Quant (e1, Range (e2, e3), gr) -> 
      eval_grab_int e2 st begin fun i1 st1 -> 
        eval_grab_int e3 st1 (fun i2 st2 -> times_quant gr e1 i1 i2 st2)
      end
    
    | Quant (e1, Star, gr) -> times_quant_while gr e1 0 st
    | Quant (e1, Plus, gr) -> times_quant_while gr e1 1 st
    | Quant (e1, Opt, gr) -> times_quant gr e1 0 1 st
    
    | Mod lst -> List.fold_left begin fun a -> function
      | Def (access, false, (PId s, _), e1, _), _ -> 
        def_access access s e1 a
        |> M.set s (VImm (C.of_mod e1 a))
      | Def (access, false, (PCat ((PId s, z) :: t), y), e1, _), _ -> 
        let e2 = As ("", (PCat t, y), e1), z in
        def_access access s e2 a
        |> M.set s (VImm (C.of_mod e1 a))
        (* opens are possessive - consider changing *)
      | Open (_, e1), _ -> 
        e e1 (State.merge_mod a st) |> unsafe_cut |> fun st1 -> 
          Module.def_open (!: st1 |> fst |> to_mod) a
      | _ -> a
      end (M.make st) lst |> fun m -> lit (VMod m) st
    
    | Access (e1, s) -> 
      e e1 st >>= fun st1 -> 
        let v, _ = !: st1 in
        begin match v with
          | VMod m -> 
            begin try e (Module.acc s m) (merge_mod m st) with
            | Not_found -> failwith "Field not found in module" end
          
          | _ -> failwith "Type Error: Accessing a non-module"
        end <&> fun st2 -> st2 <-> st
    
    | Noncap e1 -> (e e1 *> pure) st
    | Atomic (Cat lst, _) ->
      List.fold_left (fun a e1 -> a >>= e e1 |> cut) (pure st) lst
    | Atomic e1 -> e (Atomic (Cat [e1], loc), loc) st
  
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
    let c2, c1 = V.(to_cap v2, to_cap v1) in
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

  and eval_grab_int e1 st f = e e1 st >>= fun st1 ->
    let v, st1 = !: st1 in
    f (to_int v) st1

  and choose_alt = function
    | Gre -> (<|>)
    | Rel -> alt_rel
    | Cut -> alt_cut
  
  and choose_alt_flip gr = choose_alt gr |> flip

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
  
  and def_access = function
    | Pub -> Module.def
    | Local -> fun _ _ -> identity
    | Opaq -> failwith "Values cannot be opaque"
  
  and p (px, loc) st = match px with
    | PId s -> let v, st1 = !: st in pure (set s v st1)
    | PBlank -> pure st
    | PInt i1 -> 
      let v, st1 = !: st in
      if i1 = to_int v then pure st1
      else null
    | PStr s1 -> 
      let v, st1 = !: st in
      if s1 = to_str v then pure st1
      else null
    | PCat lst -> 
      List.fold_left (fun a p1 -> a >>= (p p1)) (pure st) (List.rev lst)
    | PCapture (PId s, _) -> 
      let v, st' = !: st in
      ignore (to_cap v);
      pure (set s v st')
    | PCapture px -> 
      let v, st' = !: st in
      call (to_cap v) st' >>= p px
    | PAsc (p1, _) -> p p1 st
    | PPair ((PId s1, _), (PId s2, _)) -> 
      let v, st' = !: st in
      let l, r = to_pair v in
      pure (set s2 (VImm r) (set s1 (VImm l) st'))
    | PPair (px1, (PId s2, _)) -> 
      let v, st' = !: st in
      let l, r = to_pair v in
      call l st' >>= p px1 >>= (set s2 (VImm r) >> pure)
    | PPair ((PId s1, _), px2) -> 
      let v, st' = !: st in
      let l, r = to_pair v in
      call r (set s1 (VImm l) st') >>= p px2
    | PPair (px1, px2)  -> 
      let v, st' = !: st in
      let l, r = to_pair v in
      call l st' >>= p px1 >>= call r >>= p px2
    | PLeft (PId s, _) -> begin match !: st with
        | VLeft l, st' -> pure (set s (VImm l) st')
        | VRight _, _ -> null
        | _ -> raise (ExpectedType "Either")
      end
    | PLeft px -> begin match !: st with
        | VLeft l, st' -> call l st' >>= p px
        | VRight _, _ -> null
        | _ -> raise (ExpectedType "Either")
      end
    | PRight (PId s, _) -> begin match !: st with
        | VRight r, st' -> pure (set s (VImm r) st')
        | VLeft _, _ -> null
        | _ -> raise (ExpectedType "Either")
      end
    | PRight px -> begin match !: st with
        | VRight r, st' -> call r st' >>= p px
        | VLeft _, _ -> null
        | _ -> raise (ExpectedType "Either")
      end
    | POpen false -> 
      let v, st1 = !: st in
      update_mod (to_mod v) st1 |> pure
    | PBop (p1, ">-", p2) -> p (PRight (PPair (p1, p2), loc), loc) st
    | PList [] -> p (PLeft (PUnit, loc), loc) st
    (* FIXME: delete above when the bottom is working since it's redundant *)
    | PList plst -> p (encode_plst loc plst, loc) st
    | PUnit -> let v, st1 = !: st in to_unit v; pure st1
    
    | _ -> failwith "Unimplemented - pattern"

end
