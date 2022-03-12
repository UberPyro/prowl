open Sexplib.Conv

open Ast
open Parse_proc

module M = struct
  type 'a m = 'a  [@@deriving sexp]
end

module O = Ast.T(M)

(* let t : T.t -> O.t = function
  | S x -> S (s x)
  | E x -> E (e x)

and sp (x, _) = sp_t x
and sp_t : T.sp_t -> O.sp_t = function
  | SPSp (x, y) -> SPSp (x, ty y)
  | SPTy (x, y) -> SPTy (x, ty y)
  | SPAbst_ty  *)
