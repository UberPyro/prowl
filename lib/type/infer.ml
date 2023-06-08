open! Batteries

open Parse
open Ull
open Types

exception InferError of Metadata.Span.t * string

let rec infer ctx ((node, sp, dcl, dcr) : Ast.expr) = 
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

    | _ -> failwith "todo"
  end with UnifError msg -> raise @@ InferError (sp, msg)
