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
    | _ -> failwith "todo"
  end with UnifError msg -> raise @@ InferError (sp, msg)
