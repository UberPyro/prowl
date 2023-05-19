(* Abelian Lists *)

(* open! Batteries
open Uref

exception UIntError

(* unifiable integer expressions *)
type uint = _uint uref
and _uint = 
  | Hole of int
  | Affine of (int * uint) array * int

let add_affine = 
  Ptmap.fold @@ fun i c m -> 
    m |> Ptmap.update i @@ function
      | Some c' -> Some (c' + c)
      | None -> Some c

(* let rec substitute_lin uid expr = 
  Array.iter  *)

(* make flatten a separate function? *)

let rec count arr const = 
  Array.fold_left begin fun (m, b) (a, e) -> match uget e with
    | Hole i -> Ptmap.update i begin function
      | Some a' -> Some (a' + a)
      | None -> Some a
    end m, b
    | Affine (arr', const') -> 
      let m', b' = count arr' const' in
      add_affine m m', b + b'
  end (Ptmap.empty, const) arr

let rec unify_int u = unite ~sel:sel_int u
and sel_int i1 i2 = match i1, i2 with
  | _, Hole _ -> i1
  | Hole _, _ -> i2
  | Affine (arr1, b1), Affine (arr2, b2) -> 
    begin match solve (count arr1 b1) (count arr2 b2) with
      | None -> ()
      | Some (uid, coeff, m, b) -> 



        ()
    end;
    i1
and solve (m1, b1) (m2, b2) = 
  let diff_arr = 
    add_affine m2 (Ptmap.map Int.neg m1)
    |> Ptmap.filter (fun _ x -> x <> 0)
  and diff_b = b2 - b1 in
  match Ptmap.choose_opt diff_arr with
  | None -> 
    if diff_b = 0 then None
    else raise UIntError
  | Some (uid, coeff) -> 
    let m' = 
      Ptmap.remove uid diff_arr
      |> Ptmap.map (fun x -> -x)
    and b' = -diff_b in
    Some (uid, coeff, m', b') *)







(* let count_lin arr = 
  Array.fold_left begin fun a (c, v) -> 
    Map.modify_opt v begin function
      | Some c' -> Some (c' + c)
      | None -> Some c
    end a
  end Map.empty arr *)

(* let rec count = function *)

(* let solve i1 i2 = match i1, i2 with
  | _, Hole -> i1
  | Hole, _ -> i2 *)

(* let rec flatten_affine arr c =  *)

(* let rec unify u = unite ~sel u
and sel i1 i2 = match i1, i2 with
  | _, Hole -> i1
  | Hole, _ -> i2 *)

