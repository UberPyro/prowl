open Batteries

open Prowl
open Cli

open Ast
open Gen

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | file :: _ -> 
    File.open_in file
    |> parse
    |> ContentAst.show_p
    |> (fun str -> String.nreplace ~str ~sub:"Ast.Wrap." ~by:"")
    |> print_endline
