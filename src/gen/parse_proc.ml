open Batteries
open Sexplib

module M = struct
  open Lexing
  let position_of_sexp _ = failwith "Cannot convert sexp to position"
  let sexp_of_position loc = Sexp.List [
    Sexp.Atom (string_of_int loc.pos_lnum); 
    Sexp.Atom (string_of_int loc.pos_cnum)
  ]
  type 'a m = 'a * (position * position)  [@@deriving sexp]
end

module T = Ast.T(M)

let filter_id lst = List.filter_map identity lst
