open! Batteries
open Ucommon
open Printf

module Make(U : UNIFIABLE) = struct

  type t = t_unif uref
  and t_unif = 
    | USyntax of string * t list
    | UAtom of U.t
    | UVar of int
  
  type memo = (int, t) Hashtbl.t

  let memo_ : memo = Hashtbl.create 16
  let memo () = memo_
  let refresh_memo () = 
    (* U.refresh_memo (); *)
    Hashtbl.clear memo_

  let usyn n us = uref @@ USyntax (n, us)
  let uvar () = uref @@ UVar (unique ())
  let uatom a = uref @@ UAtom a

  let rec unify r = 
    let sel x y = match x, y with
      | UVar _, UVar _ -> x
      | UVar i, USyntax (_, us) -> 
        List.iter (occurs i) us;
        y
      | USyntax (_, us), UVar i -> 
        List.iter (occurs i) us;
        x
      | UVar _, UAtom _ -> y
      | UAtom _, UVar _ -> x
      | USyntax (elem1, args1), USyntax (elem2, args2) -> 
        if elem1 <> elem2 then
          UnifError (DifferentNames (elem1, elem2))
          |> raise;
        begin try List.iter2 unify args1 args2; x
        with Invalid_argument _ -> 
          UnifError (DifferentArity ((List.length args1), (List.length args2)))
          |> raise end
      | UAtom a1, UAtom a2 -> 
        U.unify a1 a2;
        x
      | UAtom _, USyntax (n, _) | USyntax (n, _), UAtom _ -> 
        UnifError (TermHead n) |> raise in
    Uref.unite ~sel r

  and occurs i = uget %> function
      | UVar j when i = j -> UnifError Occurs |> raise
      | UVar _ -> ()
      | USyntax (_, us) -> List.iter (occurs i) us
      | UAtom a -> U.occurs i a
  
  let rec generalize m t = match uget t with
    | UVar i -> 
        Hashtbl.find_option m i |> Option.default_delayed @@ fun () -> 
          let nu = uvar () in
          Hashtbl.add m i nu;
          nu
    | USyntax (name, us) -> 
      uref @@ USyntax (name, List.map (generalize m) us)
    | UAtom a -> uref @@ UAtom (U.generalize (U.memo ()) a)
  
  let rec pretty out = uget %> function
    | USyntax (n, []) -> fprintf out "%s" n
    | USyntax (n, us) -> 
      fprintf out "%s(" n;
      begin match us with
        | [] -> ()
        | h :: t -> 
          pretty out h;
          List.iter (fun u -> fprintf out ","; pretty out u) t
      end;
      fprintf out ")"
    | UVar j -> fprintf out "V%d" j
    | UAtom a -> U.pretty out a
  
  let rec atleast m = curry @@ Tuple2.mapn uget %> function
    | USyntax (s1, ts1), USyntax (s2, ts2) -> 
      s1 = s2 && List.for_all2 (atleast m) ts1 ts2
    | UAtom a1, UAtom a2 -> U.atleast m a1 a2
    | UVar i, UVar j -> Matcher.check m i j
    | UVar _, _ -> true
    | _, _ -> false

end
