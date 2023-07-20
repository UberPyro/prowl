open! Batteries
open Ucommon
open Printf

module Make(U : UNIFIABLE) = struct

  type t = t_unif uref
  and t_unif = 
    | USyntax of U.t * t list
    | UVar of int
  
  type memo = (int, t) Hashtbl.t

  let memo_ : memo = Hashtbl.create 16
  let memo () = memo_
  let refresh_memo () = 
    (* U.refresh_memo (); *)
    Hashtbl.clear memo_

  let usyn n us = uref @@ USyntax (n, us)
  let uvar () = uref @@ UVar (unique ())

  let rec unify r = 
    let sel x y = match x, y with
      | UVar _, UVar _ -> x
      | UVar i, USyntax (_, us) -> 
        List.iter (occurs i) us;
        y
      | USyntax (_, us), UVar i -> 
        List.iter (occurs i) us;
        x
      | USyntax (elem1, args1), USyntax (elem2, args2) -> 
        U.unify elem1 elem2; 
        try List.iter2 unify args1 args2; x
        with Invalid_argument _ -> 
          UnifError (
            sprintf "Cannot unify terms of arity [%d] and [%d]"
            (List.length args1) (List.length args2))
          |> raise in
    Uref.unite ~sel r

  and occurs i = uget %> function
      | UVar j when i = j -> 
        UnifError "Cannot unify a variable with syntax that contains it"
        |> raise
      | UVar _ -> ()
      | USyntax (n, us) -> 
        U.occurs i n;
        List.iter (occurs i) us
  
  let rec generalize m t = match uget t with
    | UVar i -> 
        Hashtbl.find_option m i |> Option.default_delayed @@ fun () -> 
          let nu = uvar () in
          Hashtbl.add m i nu;
          nu
    | USyntax (name, us) -> 
      uref @@ USyntax (U.generalize (U.memo ()) name, List.map (generalize m) us)
  
  let rec pretty out = uget %> function
    | USyntax (n, us) -> 
      U.pretty out n;
      begin match us with
        | [] -> ()
        | h :: t -> 
          pretty out h;
          List.iter (fun u -> fprintf out " "; pretty out u) t
      end
    | UVar j -> fprintf out "%d*" j

end
