open! Batteries
open Printf

open Metadata
open Ull
open Types
open Parse

exception InferError of Span.t * string

let rec infer (ctx : (string, bool * dc * dc) Ouro.t) ((node, sp, dcl, dcr) : Ast.expr) = 
  try begin
    match node with
    | Bop ((_, sp1, dcl1, dcr1 as e1), Aop _, (_, sp2, dcl2, dcr2 as e2)) -> 
      begin try
        unify_dc (no_dc ()) dcl1;
        unify_dc (no_dc () <: TLit TInt) dcr1;
      with UnifError msg -> raise @@ InferError (sp1, msg)
      end;
      begin try
        unify_dc (no_dc ()) dcl2;
        unify_dc (no_dc () <: TLit TInt) dcr2;
      with UnifError msg -> raise @@ InferError (sp2, msg)
      end;
      let dc3 = mk_dc () in
      unify_dc dc3 dcl;
      unify_dc (dc3 <: TLit TInt) dcr;
      infer ctx e1;
      infer ctx e2;

    | Bop ((_, sp1, dcl1, dcr1 as e1), Cop _, (_, sp2, dcl2, dcr2 as e2)) -> 
      begin try
        unify_dc (no_dc ()) dcl1;
        unify_dc (no_dc () <: TLit TInt) dcr1;
      with UnifError msg -> raise @@ InferError (sp1, msg)
      end;
      begin try
        unify_dc (no_dc ()) dcl2;
        unify_dc (no_dc () <: TLit TInt) dcr2;
      with UnifError msg -> raise @@ InferError (sp2, msg)
      end;
      let dc3 = mk_dc () in
      unify_dc dc3 dcl;
      unify_dc (dup_dc dc3) dcr;
      infer ctx e1;
      infer ctx e2;
    
    | SectLeft (Aop _, (_, sp2, dcl2, dcr2 as e2)) -> 
      begin try
        unify_dc (no_dc ()) dcl2;
        unify_dc (no_dc () <: TLit TInt) dcr2;
      with UnifError msg -> raise @@ InferError (sp2, msg)
      end;
      let dc3 = mk_dc () <: TLit TInt in
      unify_dc dc3 dcl;
      unify_dc dc3 dcr;
      infer ctx e2;
    
    | SectLeft (Cop _, (_, sp2, dcl2, dcr2 as e2)) -> 
      begin try
        unify_dc (no_dc ()) dcl2;
        unify_dc (no_dc () <: TLit TInt) dcr2;
      with UnifError msg -> raise @@ InferError (sp2, msg)
      end;
      let dc3 = mk_dc () in
      unify_dc (dc3 <: TLit TInt) dcl;
      unify_dc (dup_dc dc3) dcr;
      infer ctx e2;
    
    | SectRight ((_, sp1, dcl1, dcr1 as e1), Aop _) -> 
      begin try
        unify_dc (no_dc ()) dcl1;
        unify_dc (no_dc () <: TLit TInt) dcr1;
      with UnifError msg -> raise @@ InferError (sp1, msg)
      end;
      let dc3 = mk_dc () <: TLit TInt in
      unify_dc dc3 dcl;
      unify_dc dc3 dcr;
      infer ctx e1;
    
    | SectRight ((_, sp1, dcl1, dcr1 as e1), Cop _) -> 
      begin try
        unify_dc (no_dc ()) dcl1;
        unify_dc (no_dc () <: TLit TInt) dcr1;
      with UnifError msg -> raise @@ InferError (sp1, msg)
      end;
      let dc3 = mk_dc () in
      unify_dc (dc3 <: TLit TInt) dcl;
      unify_dc (dup_dc dc3) dcr;
      infer ctx e1;
    
    | Sect Aop _ -> 
      let dc3 = mk_dc () <: TLit TInt in
      unify_dc (dc3 <: TLit TInt) dcl;
      unify_dc dc3 dcr;
    
    | Sect Cop _ -> 
      let dc3 = mk_dc () in
      unify_dc (dc3 <: TLit TInt <: TLit TInt) dcl;
      unify_dc (dup_dc dc3) dcr;
    
    | Uop ((_, _, dcl1, dcr1 as e1), Dag) -> 
      unify_dc dcr1 dcl;
      unify_dc dcl1 dcr;
      infer ctx e1;
    
    | Dop ((_, _, dcl1, dcr1 as e1), Ponder, (_, _, dcl2, dcr2 as e2)) -> 
      let dcl_low, dcl_high = dcl in
      let dcr_low, dcr_high = dcr in
      let dcl1_low, dcl1_high = dcl1 in
      let dcr1_low, dcr1_high = dcr1 in
      let dcl2_low, dcl2_high = dcl2 in
      let dcr2_low, dcr2_high = dcr2 in
      unify_c dcl1_high dcl2_low;
      unify_c dcl1_low dcl_low;
      unify_c dcl2_high dcl_high;
      unify_c dcr1_high dcr2_low;
      unify_c dcr1_low dcr_low;
      unify_c dcr2_high dcr_high;
      infer ctx e1;
      infer ctx e2;
    
    | Dop ((_, _, dcl1, dcr1 as e1), Tensor, (_, _, dcl2, dcr2 as e2)) -> 
      let dcl_low, dcl_high = dcl in
      let dcr_low, dcr_high = dcr in
      let dcl1_low, dcl1_high = dcl1 in
      let dcr1_low, dcr1_high = dcr1 in
      let dcl2_low, dcl2_high = dcl2 in
      let dcr2_low, dcr2_high = dcr2 in
      unify_c dcl_low dcl1_low;
      unify_c dcl_low dcl2_low;
      unify_c dcr_low dcr1_low;
      unify_c dcr_low dcr2_low;
      let (dsl_low, dsl_high), dcl' = upop dcl_high in
      let (dsr_low, dsr_high), dcr' = upop dcr_high in
      let (dsl1_low, dsl1_high), dcl1' = upop dcl1_high in
      let (dsr1_low, dsr1_high), dcr1' = upop dcr1_high in
      let (dsl2_low, dsl2_high), dcl2' = upop dcl2_high in
      let (dsr2_low, dsr2_high), dcr2' = upop dcr2_high in
      unify_c dcl' dcl1';
      unify_c dcl' dcl2';
      unify_c dcr' dcr1';
      unify_c dcr' dcr2';
      unify_s dsl1_high dsl2_low;
      unify_s dsl1_low dsl_low;
      unify_s dsl2_high dsl_high;
      unify_s dsr1_high dsr2_low;
      unify_s dsr1_low dsr_low;
      unify_s dsr2_high dsr_high;
      infer ctx e1;
      infer ctx e2;
    
    | Dop ((_, _, dcl1, dcr1 as e1), Pick, (_, _, dcl2, dcr2 as e2)) -> 
      unify_dc dcr dcr1;
      unify_dc dcr dcr2;
      let dcl_low, dcl_high = dcl in
      let dcl1_low, dcl1_high = dcl1 in
      let dcl2_low, dcl2_high = dcl2 in
      unify_c dcl1_high dcl2_low;
      unify_c dcl1_low dcl_low;
      unify_c dcl2_high dcl_high;
      infer ctx e1;
      infer ctx e2;
      
    | Dop ((_, _, dcl1, dcr1 as e1), Fork, (_, _, dcl2, dcr2 as e2)) -> 
      unify_dc dcl dcl1;
      unify_dc dcl dcl2;
      let dcr_low, dcr_high = dcr in
      let dcr1_low, dcr1_high = dcr1 in
      let dcr2_low, dcr2_high = dcr2 in
      unify_c dcr_low dcr1_low;
      unify_c dcr_low dcr2_low;
      let (dsr_low, dsr_high), dcr' = upop dcr_high in
      let (dsr1_low, dsr1_high), dcr1' = upop dcr1_high in
      let (dsr2_low, dsr2_high), dcr2' = upop dcr2_high in
      unify_c dcr' dcr1';
      unify_c dcr' dcr2';
      unify_s dsr1_high dsr2_low;
      unify_s dsr1_low dsr_low;
      unify_s dsr2_high dsr_high;
      infer ctx e1;
      infer ctx e2;
    
    | Dop ((_, _, dcl1, dcr1 as e1), Guess, (_, _, dcl2, dcr2 as e2)) -> 
      unify_dc dcl dcl1;
      unify_dc dcl dcl2;
      let dcr_low, dcr_high = dcr in
      let dcr1_low, dcr1_high = dcr1 in
      let dcr2_low, dcr2_high = dcr2 in
      unify_c dcr1_high dcr2_low;
      unify_c dcr1_low dcr_low;
      unify_c dcr2_high dcr_high;
      infer ctx e1;
      infer ctx e2;
    
    | Dop ((_, _, dcl1, dcr1 as e1), Cross, (_, _, dcl2, dcr2 as e2)) -> 
      unify_dc dcr dcr1;
      unify_dc dcr dcr2;
      let dcl_low, dcl_high = dcl in
      let dcl1_low, dcl1_high = dcl1 in
      let dcl2_low, dcl2_high = dcl2 in
      unify_c dcl_low dcl1_low;
      unify_c dcl_low dcl2_low;
      let (dsl_low, dsl_high), dcl' = upop dcl_high in
      let (dsl1_low, dsl1_high), dcl1' = upop dcl1_high in
      let (dsl2_low, dsl2_high), dcl2' = upop dcl2_high in
      unify_c dcl' dcl1';
      unify_c dcl' dcl2';
      unify_s dsl1_high dsl2_low;
      unify_s dsl1_low dsl_low;
      unify_s dsl2_high dsl_high;
      infer ctx e1;
      infer ctx e2;
    
    | Dop ((_, _, dcl1, dcr1 as e1), Jux, (_, _, dcl2, dcr2 as e2)) -> 
      unify_dc dcl dcl1;
      unify_dc dcr dcr2;
      unify_dc dcr1 dcl2;
      infer ctx e1;
      infer ctx e2;
    
    | Dop ((_, _, dcl1, dcr1 as e1), Contra, (_, _, dcl2, dcr2 as e2)) -> 
      unify_dc dcl dcr1;
      unify_dc dcr dcl2;
      unify_dc dcl1 dcr2;
      infer ctx e1;
      infer ctx e2;
    
    | Dop ((_, _, dcl1, dcr1 as e1), Union, (_, _, dcl2, dcr2 as e2)) -> 
      unify_dc dcl dcl1;
      unify_dc dcl dcl2;
      unify_dc dcr dcr1;
      unify_dc dcr dcr2;
      infer ctx e1;
      infer ctx e2;
    
    | Nop (Gen | Fab) -> 
      let dc3 = mk_dc () in
      unify_dc dcl dc3;
      unify_dc dcr (dup_dc dc3);
    
    | Nop Elim -> 
      let s1 = mk_ds () in
      let s2 = mk_ds () in
      let dc3 = mk_dvoid () in
      unify_dc dcl (dc3 <:: s1 <:: s2);
      unify_dc dcr (dc3 <:: s2 <:: s1);
    
    | Nop Cmp -> 
      let dc3 = mk_dc () in
      unify_dc dcl (dc3 <: TLit TInt <: TLit TInt);
      unify_dc dcr (dup_dc @@ dup_dc dc3);
    
    | Nop Dup -> 
      let dc3 = mk_dc () in
      let var = TMeta (unique ()) in
      unify_dc dcl (dc3 <: var);
      unify_dc dcr (dc3 <: var <: var);
    
    | Nop Zap -> 
      let dc3 = mk_dc () in
      unify_dc dcl (dc3 <: TMeta (unique ()));
      unify_dc dcr dc3
    
    | Nop Swap -> 
      let dc3 = mk_dc () in
      let v1 = TMeta (unique ()) in
      let v2 = TMeta (unique ()) in
      unify_dc dcl (dc3 <: v1 <: v2);
      unify_dc dcr (dc3 <: v2 <: v1);
    
    | Nop Cons -> 
      let r = mk_dc () in
      let s = mk_dc () in
      let t = mk_dc () in
      let v = TMeta (unique ()) in
      unify_dc dcl (t <: v <: TCon (TQuote, r <: v, s));
      unify_dc dcr (t <: TCon (TQuote, r, s));
    
    | Nop Dip -> 
      let r = mk_dc () in
      let s = mk_dc () in
      let v = TMeta (unique ()) in
      unify_dc dcl (r <: v <: TCon (TQuote, r, s));
      unify_dc dcr (s <: v);
    
    | Nop Cat -> 
      let r = mk_dc () in
      let s = mk_dc () in
      let t = mk_dc () in
      let q = mk_dc () in
      unify_dc dcl (q <: TCon (TQuote, r, s) <: TCon (TQuote, s, t));
      unify_dc dcr (q <: TCon (TQuote, r, t));
    
    | Nop Unit -> 
      let r = mk_dc () in
      let s = mk_dc () in
      let v = TMeta (unique ()) in
      unify_dc dcl (r <: v);
      unify_dc dcr (r <: TCon (TQuote, s, s <: v));
    
    | Nop DivMod -> 
      let r = mk_dc () <: TLit TInt <: TLit TInt in
      unify_dc dcl r;
      unify_dc dcr r;
    
    | Nop Lin -> 
      let r = mk_dc () in
      let s = mk_dc () in
      let t = mk_dc () in
      unify_dc dcl (t <: TCon (TList, r, s) <: TCon (TQuote, r, s));
      unify_dc dcr (t <: TCon (TList, r, s));
    
    | Nop Bin -> 
      let r = mk_dc () in
      let s = mk_dc () in
      let t = mk_dc () in
      unify_dc dcl (t 
        <: TCon (TTree, r, s)
        <: TCon (TTree, r, s)
        <: TCon (TQuote, r, s));
      unify_dc dcr (t <: TCon (TTree, r, s));
    
    | Nop Parse -> 
      let r = mk_dc () in
      unify_dc dcl (r <: TLit TInt);
      unify_dc dcr (r <: TLit TString);
    
    | Nop Show -> 
      let r = mk_dc () in
      unify_dc dcl (r <: TLit TString);
      unify_dc dcr (r <: TLit TInt);
    
    | Lit Int _ -> 
      let r = mk_dc () in
      unify_dc dcl r;
      unify_dc dcr (r <: TLit TInt);
    
    | Lit String _ -> 
      let r = mk_dc () in
      unify_dc dcl r;
      unify_dc dcr (r <: TLit TString);
    
    | Lit Quote (_, _, dclq, dcrq as e) -> 
      let r = mk_dc () in
      unify_dc dcl r;
      unify_dc dcr (r <: TCon (TQuote, dclq, dcrq));
      infer ctx e;
    
    | Lit List es -> 
      let r = mk_dc () in
      unify_dc dcl r;
      unify_dc dcr (r <: TCon (TList, mk_dc (), mk_dc ()));
      es |> List.iter begin fun (_, sp', dcl', dcr' as e') -> 
        infer ctx e'; 
        try unify_dc dcr (r <: TCon (TList, dcl', dcr'))
        with UnifError msg -> raise @@ InferError (sp', msg)
      end;
    
    | Var k -> 
      begin match Ouro.find_rec_opt k ctx with
        | Some ((generalized, dcl1, dcr1), _) -> 
          let transform = 
            if generalized then freshen_dc @@ Gen.mk_cache () 
            else Fun.id in
          unify_dc dcl (transform dcl1);
          unify_dc dcr (transform dcr1);
        | None -> 
          let msg = sprintf "Cannot find unbound variable [%s]" k in
          raise @@ UnifError msg
      end
    
    | Let (stmts, e) -> 
      let ctx' = stmts_rec false ctx stmts in
      infer ctx' e

    | _ -> failwith "todo"
  end with UnifError msg -> raise @@ InferError (sp, msg)

and stmts_rec generalized ctx stmts = 
  let unwrap (Ast.Def (s, (_, _, l, r)), _) = s, (false, l, r) in
  let ctx' = Ouro.insert_many (List.map unwrap stmts) ctx in
  List.iter (fun (Ast.Def (_, e), _) -> infer ctx' e) stmts;
  Ouro.vmap (fun (_, l, r) -> generalized, l, r) ctx'

let top_stmts = stmts_rec true
