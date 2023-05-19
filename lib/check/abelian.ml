(* Integer unification *)

open! Batteries
open Uref

type uz = _uz uref
and _uz = 
  | Var of int * int  (* coeff, uid *)
  | Affine of uz array * int

let add_affine = 
  Ptmap.fold @@ fun i c m -> 
    m |> Ptmap.update i @@ function
      | Some c' -> Some (c' + c)
      | None -> Some c

let rec count arr c = 
  Array.fold begin fun (m, b) uz -> match uget uz with
    | Var (a, i) -> Ptmap.update i begin function
      | Some a' -> Some (a' + a)
      | None -> Some a
    end m, b
    | Affine (arr', const') -> 
      let m', b' = count arr' const' in
      add_affine m m', b + b'
  end (Ptmap.empty, c) arr

let solve (m1, b1) (m2, b2) = 
  let diff_arr = 
    add_affine m2 (Ptmap.map Int.neg m1)
    |> Ptmap.filter (fun _ x -> x <> 0)
  and diff_b = b2 - b1 in
  match Ptmap.choose_opt diff_arr with
  | None -> 
    if diff_b = 0 then None
    else failwith "Type Error"
  | Some (uid, coeff) -> 
    let m' = 
      Ptmap.remove uid diff_arr
      |> Ptmap.map (fun x -> -x)
    and b' = -diff_b in
    Some (uid, coeff, m', b')

(* let rec unify_int u = unite ~sel:sel_int u
and sel_int i1 i2 = match i1, i2 with
  | _, Var _ -> i1
  | Var _, _ -> i2
  | Affine (arr1, b1), Affine (arr2, b2) -> 
    begin match solve (count arr1 b1) (count arr2 b2) with
      | None -> ()
      | Some (uid, coeff, m, b) -> 



        ()
    end;
    i1 *)
