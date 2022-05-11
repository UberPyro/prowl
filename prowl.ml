open Batteries
module O = BatOptParse.Opt

open Lib
open Cli

open Gen
open Ast

let lex_out = flag "output lexemes" "lex"
let ast_out = flag "output ast as ocaml ADTs" "ast"

let compile = List.iter begin fun file -> 
  if O.get lex_out then File.open_in file |> lex;
  let ast = parse (File.open_in file) in
  if O.get ast_out then print_endline (show_program ast)
end

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | lst -> compile lst
