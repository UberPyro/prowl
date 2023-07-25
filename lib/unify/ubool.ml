open! Batteries
open Printf
open Either
open Ucommon

module type Constant = sig
  type t
  val and_const : t -> t -> t
  val xor_const : t -> t -> t
  val one : t
  val zero : t
  val to_string : t -> string
end

module Make(C : Constant) = struct
  
  type boolean = boolean_ ref
  and boolean_ = 
    | BExpr of boolean list list
    | BVar of int
    | BConst of C.t
  type ubool = boolean list list uref
  type t = ubool

  type memo = (int, boolean) Hashtbl.t
  let memo_ : memo = Hashtbl.create 16
  let memo () = memo_
  let refresh_memo () = Hashtbl.clear memo_
  
  let bfresh_ () = [[ref (BVar (unique ()))]]
  let bfresh () = uref (bfresh_ ())
  
  let mul_basic b1 b2 = 
    List.concat_map (fun p1 -> List.map ((@) p1) b2) b1
  
  let coal_mul bs = 
    match bs |> List.partition_map @@ fun x -> match !x with
      | BConst c -> Right c
      | _ -> Left x with
    | vs, [] -> vs
    | vs, cs -> 
      let c' = List.fold C.and_const C.one cs in
      if c' = C.zero then [ref (BConst (C.zero))]
      else List.sort_unique Stdlib.compare @@
        if c' = C.one then vs
        else ref (BConst c') :: vs

  let coal_add b = 
    match b |> List.partition_map @@ fun x -> match List.map (!) x with
      | [BConst c] -> Right c
      | _ -> Left x with
    | vs, [] -> vs
    | vs, cs -> [ref (BConst (List.fold C.xor_const C.zero cs))] :: vs

  let mul_idem b1 b2 = 
    mul_basic b1 b2 |> List.map @@ List.sort_uniq Stdlib.compare %> coal_mul

  let cancel_dups = 
    let[@tail_mod_cons] rec go = function
      | h1 :: h2 :: t when h1 = h2 -> go t
      | h :: t -> h :: go t
      | [] -> [] in
    List.sort Stdlib.compare %> go

  let add_xor b1 b2 = b1 @ b2 |> cancel_dups |> coal_add

  let rec normalize_ = function
    | [] -> [[ref @@ BConst C.zero]]
    | lst -> List.map (fun y -> match y with
      | [] -> [ref @@ BConst C.one]
      | r -> List.map (fun x -> match !x with
        | BExpr e -> ref @@ BExpr (normalize_ e)
        | _ -> x) r) lst
  
  let rec simp b = 
    List.map begin fun bs -> 
      match List.partition_map begin fun x -> match !x with
        | BExpr e -> 
          let e' = simp e in
          x := BExpr e';
          Right e'
        | _ -> Left x
      end bs with
      | xs, [] -> [xs]
      | xs, es -> List.fold mul_idem [xs] es
    end b
    |> List.flatten |> cancel_dups |> coal_add |> normalize_
  
  let tally m v = 
    m |> Hashtbl.modify_opt v @@ function
      | Some i -> Some (i + 1)
      | None -> Some 1
  
  let rec pretty_ubool out = uget %> simp %> pretty_ubool_ out
    and pretty_ubool_ out = function
      | [] -> fprintf out "%s" @@ C.to_string C.zero
      | h :: t -> 
        pretty_ubool_inner out h;
        List.iter (fun x -> fprintf out " ^ "; pretty_ubool_inner out x) t
    and pretty_ubool_inner out = function
      | [] -> fprintf out "%s" @@ C.to_string C.one
      | h :: t -> 
        pretty_boolean out h;
        List.iter (fun x -> fprintf out " & "; pretty_boolean out x) t
    and pretty_boolean out = (!) %> pretty_boolean_ out
    and pretty_boolean_ out = function
      | BExpr e -> pretty_ubool_ out e
      | BVar i -> fprintf out "X%d" i
      | BConst c -> fprintf out "%s" @@ C.to_string c
    let pretty_print printer in_ = 
      let out = IO.output_string () in
      printer out in_;
      IO.close_out out
  
  let get_var b = 
    let m = Hashtbl.create 16 in
    b |> List.iter (List.iter ((!) %> function
      | BVar i -> tally m i
      | _ -> ()));
    Hashtbl.bindings m
    |> List.sort (fun x y -> Stdlib.compare (snd y) (snd x))
    |> List.map fst |> function
      | h :: _ -> h  (* variable with *most* counts *)
      | [] -> raise @@ UnifError "Determinism Error!"
  
  let extract b = 
    let v = get_var b in
    let v_ref = List.flatten b |> List.find ((!) %> (=) (BVar v)) in
    let has_v, no_v = List.partition (List.map (!) %> List.mem (BVar v)) b in
    let t1 = List.map (List.filter ((!) %> (<>) (BVar v))) has_v in
    v_ref, t1, no_v
  
  let rec solve t0 = 
    (* print_endline @@ "solving: " ^ (pretty_print pretty_ubool_ t0); *)
    let t = simp t0 in
    match t with
    | [] -> ()
    | [[x]] when !x = BConst C.zero -> ()
    | _ -> 
        let x, t1, t2 = extract t in
        let t1c = add_xor t1 [[]] in
        solve (mul_idem t1c t2);
        x := BExpr (add_xor (mul_idem t1c (bfresh_ ())) t2)
  
  let size = List.flatten %> List.length
  let smaller x y = if size y > size x then y else x
  let unify u v = try unite ~sel:begin fun x y -> 
    solve (x @ y); 
    smaller (simp x) (simp y)
  end u v with
  | UnifError _ -> 
    let msg = sprintf
      "Cannot unify booleans [%s] and [%s]"
      (pstr pretty_ubool u)
      (pstr pretty_ubool v) in
    UnifError msg |> raise

  let occurs _ _ = ()
  let rec generalize m t = t |> uget |> generalize_ m |> uref
  and generalize_ m t = List.map (List.map (generalize_boolean m)) t
  and generalize_boolean m t = match !t with
    | BVar i -> 
      Hashtbl.find_option m i |> Option.default_delayed @@ fun () -> 
        let nu = ref @@ BVar (unique ()) in
        Hashtbl.add m i nu;
        nu
    | BExpr b -> ref @@ BExpr (generalize_ m b)
    | _ -> t

  let pretty = pretty_ubool
  
  let rec atleast_ m t1 t2 = 
    List.for_all2 (List.for_all2 (atleast_inner m)) t1 t2
  
  and atleast_inner m = curry @@ Tuple2.mapn (!) %> function
    | BExpr e1, BExpr e2 -> atleast_ m e1 e2
    | BConst c1, BConst c2 -> c1 = c2
    | BVar i, BVar j -> Matcher.check m i j
    | BVar _, _ -> true
    | _ -> false
  
  let atleast m t1 t2 = 
    atleast_ m (t1 |> uget %> simp) (t2 |> uget %> simp)

end
