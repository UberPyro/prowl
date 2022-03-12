open Batteries
(* open Sexplib *)
module O = BatOptParse.Opt

open Lib
open Cli

let sexp_out = flag "output AST in s-expressions" "sexp"
let lex_out = flag "output lexemes" "lex"

let () = 
  match P.parse_argv op with
  | [] -> P.usage op ()
  | lst -> 
    (* let is_sexp = O.get sexp_out in *)
    List.map begin fun file -> 
      (* let name, _ = Filename.split_extension file in *)
      if O.get lex_out then File.open_in file |> Gen.lex;
      let ast = File.open_in file |> Gen.ast in
      (* if is_sexp then
        ast
        |> Ast.sexp_of_stmt
        |> Sexp.to_string_hum
        |> print_endline *)
        ignore ast
    end lst |> ignore
