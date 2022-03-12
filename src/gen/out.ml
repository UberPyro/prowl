open Sexplib.Conv

open Ast
open Parse_proc

module M = struct
  type 'a m = 'a  [@@deriving sexp]
end

module O = Ast.T(M)

(* let t : T.t -> O.t = function
  | S x -> S (s x)
  | E x -> E e x)

and sp (sp_t, ) *)
