open Batteries
module O = BatOptParse.Opt

open Lib
open Cli

let lex_out = flag "output lexemes" "lex"
(* let ast_out = flag "output ast as ocaml ADTs" "ast" *)

let compile = List.map begin fun file -> 
  if O.get lex_out then File.open_in file |> Gen.lex;
  let ast = Gen.parse (File.open_in file) in
  ignore ast
end
