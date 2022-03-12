open Batteries
open Sexplib
(* open Sexplib *)
module O = BatOptParse.Opt

open Lib
open Cli
open Parse_proc

let lex_out = flag "output lexemes" "lex"
let ast_out = flag "output ast as s-expression" "ast"

let () = 
  match P.parse_argv op with
  | [] -> P.usage op ()
  | lst -> 
    List.map begin fun file -> 
      (* let name, _ = Filename.split_extension file in *)
      if O.get lex_out then File.open_in file |> Gen.lex;
      let ast = File.open_in file |> Gen.ast in
      if O.get ast_out then
        T.sexp_of_t ast
        |> Sexp.to_string_hum
        |> print_endline
    end lst |> ignore
