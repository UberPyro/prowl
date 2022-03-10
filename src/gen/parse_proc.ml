open Batteries

module M = struct
  type 'a m = 'a * (Lexing.position * Lexing.position)
end

module T = Ast.T(M)

let filter_id lst = List.filter_map identity lst
