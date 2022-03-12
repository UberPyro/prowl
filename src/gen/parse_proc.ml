open Batteries
(* open Sexplib *)

module M = struct
  open Lexing
  (* let position_of_sexp _ = failwith "Cannot convert sexp to position"
  let sexp_of_position loc = Sexp.List [
    Sexp.Atom (string_of_int loc.pos_lnum); 
    Sexp.Atom (string_of_int loc.pos_cnum)
  ] *)
  let sexp_of_m f (a, _) = f a
  let m_of_sexp _ = failwith "Cannot convert sexp into location"
  type 'a m = 'a * (position * position)
end

module T = Ast.T(M)

let filter_id lst = List.filter_map identity lst
