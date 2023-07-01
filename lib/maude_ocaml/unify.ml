open! Batteries
open Swig

type fmod = Maude.c_enum_value Swig.c_obj_t
type term = Maude.c_enum_value Swig.c_obj_t

let parse_mod prog name = 
  Maude._input @@ C_string prog |> ignore;
  Maude._getModule @@ C_string name

let parse_term fm s = 
  C_list [fm; Maude._tokenize @@ C_string s]
  |> Maude._Module_parseTerm

let obscure_term = Fun.id
let expose_term = Fun.id

let unify fm t1 t2 = 
  let up = Maude._Module_unify @@ C_list [
    fm;
    let empty = Maude._new_TermPairVector @@ C_list [] in
    [|
      Maude._new_TermPair @@ C_list [
        Maude._Module_parseTerm @@ C_list [fm; t1];
        Maude._Module_parseTerm @@ C_list [fm; t2];
      ];
    |]
    |> Maude.array_to_vector empty Fun.id
    |> Maude._new_TermPairVector
  ] in
  List.unfold () @@ fun () -> 
    let next = Maude._UnificationProblem___next up in
    match next with
    | C_void -> None
    | _ -> Some (
      List.unfold (Maude._Substitution_iterator next) begin function
        | C_void -> None
        | it -> 
          let v = Maude._Iterator_getValue it in
          let w = Maude._Iterator_getVariable it in
          let w_pp = 
            Maude._Term_getVarName w
            |> function[@warning "-8"] C_string s -> s in
          Some ((w_pp, v), Maude._Iterator_nextAssignment it)
      end, ())

let show_term t = 
  C_list [t; C_int 0]
  |> Maude._Term_prettyPrint
  |> function[@warning "-8"] C_string s -> s

let term_sort t = 
  match[@warning "-8"] Maude._Sort_to_string @@ Maude._Term_getSort t with
  | C_string s -> s

(* term building *)
type symbol = Maude.c_enum_value Swig.c_obj_t

let mk_symmap m = 
  let symvec = Maude._Module_getSymbols m in
  let[@warning "-8"] (C_int len) = Maude._SymbolVector_size symvec in
  let syms = 
    List.unfold 0 @@ fun i -> 
      if i < len
      then Some (
        Maude._SymbolVector___getitem__ @@ C_list [symvec; C_int i], 
        i + 1
      ) else None in
  List.fold_left begin fun xs x -> 
    let[@warning "-8"] (C_string k) = Maude._Symbol_to_string x in
    Map.add k x xs
  end Map.empty syms

let build name symmap ts = 
  let sym = Map.find name symmap in
  Maude._Symbol_makeTerm @@ C_list [sym; C_list ts]

let break t = 
  let ai = Maude._Term_arguments t in
  let[@warning "-8"] (C_string s) = Maude._Term_symbol t in
  let ts = List.unfold ai @@ fun it -> 
    let[@warning "-8"] (C_bool b) = Maude._ArgumentIterator_valid it in
    if b then 
      let x = Some (Maude._ArgumentIterator_argument it, it) in
      Maude._ArgumentIterator___next it |> ignore;
      x
    else None in
  s, ts

(* let rec you_are_a = function
| Swig.C_void -> print_endline "void"
| C_bool true -> print_endline "bool: true" 
| C_bool false -> print_string "bool: false" 
| C_char c -> print_string "char: "; print_char c; print_newline ()
| C_uchar c -> print_string "uchar: "; print_char c; print_newline ()
| C_short s -> Printf.printf "short: %d\n" s
| C_ushort s -> Printf.printf "ushort: %d\n" s
| C_int i -> Printf.printf "int: %d\n" i
| C_uint _ -> Printf.printf "uint: "
| C_int32 _ -> print_endline "int32"
| C_int64 _ -> print_endline "int64"
| C_float f -> print_endline "float: "; print_float f; print_newline ()
| C_double f -> print_endline "double: "; print_float f; print_newline ()
| C_ptr _ -> print_endline "ptr"
| C_array arr -> print_endline "arr: "; Array.iter you_are_a arr
| C_list lst -> print_endline "lst: "; List.iter you_are_a lst
| C_obj _ -> print_endline "obj"
| C_string s -> Printf.printf "string: %s" s
| C_enum _ -> print_endline "enum"
| C_director_core (c1, r) -> 
  print_endline "director_core: ";
  you_are_a c1;
  match !r with
  | None -> print_endline "...with nothing"
  | Some c2 -> print_endline "...with something: "; you_are_a c2 *)

(* [@@@warning "-10"]
let (%>) f g x = g (f x);;

let x = Maude._new_TermPair;;

let _ = 
  Maude._input @@ C_string Assoc_prog.prog |> ignore;
  let m = Maude._getModule @@ C_string "ASSOC" in
  let up = Maude._Module_unify @@ C_list [
    m;
    (* Maude._Module_parseTerm *)

    (* Maude._new_TermPair @@ C_list [
      Maude._Module_parseTerm @@ C_list [m; Maude._tokenize @@ C_string "X:A ++ Y:A"];
      Maude._Module_parseTerm @@ C_list [m; Maude._tokenize @@ C_string "a ++ b ++ c ++ d ++ e ++ f"]
    ]; *)
  begin
    let empty = Maude._new_TermPairVector @@ C_list [] in
    Maude._new_TermPairVector @@ Maude.array_to_vector empty Fun.id @@ [|
      Maude._new_TermPair @@ C_list [
        Maude._Module_parseTerm @@ C_list [m; Maude._tokenize @@ C_string "X:A ++ Y:A"];
        Maude._Module_parseTerm @@ C_list [m; Maude._tokenize @@ C_string "a ++ b ++ c ++ d ++ e ++ f"];
      ];
    |]
  end
    
    (* C_int 1; *)
  ] in
  for _ = 0 to 6 do  (* number of solutions *)
    let it_ = ref @@ Maude._Substitution_iterator @@ Maude._UnificationProblem___next up in
    let it = !it_ in
    for _ = 0 to 1 do  (* number of variables *)
      let v = Maude._Iterator_getValue it in
      let w = Maude._Iterator_getVariable it in
      let v_pp = Maude._Term_prettyPrint @@ C_list [v; C_int 0] |> function[@warning "-8"] C_string s -> s in
      let w_pp = Maude._Term_getVarName w |> function[@warning "-8"] C_string s -> s in
      Printf.printf "%s --> %s" w_pp v_pp;
      print_newline ();
      it_ := Maude._Iterator_nextAssignment it;
    done;
    print_newline ();
  done *)


  (* let _ = Maude._UnificationProblem___next up in *)
  
  (* let rec go x = 
    print_endline "cool beans";
    let y = Maude._UnificationProblem___next x in
    go y in
  go up *)
    
    (* us |> List.iter @@
      Maude._Term_prettyPrint %> function[@warning "-8"]
        C_string s -> print_endline s *)
