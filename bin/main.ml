open Batteries

open Prowl
open Cli

open Ast
open Gen

let ast_out = flag "dump ast" "ast"
let typecheck = flags "typecheck program" "check" 't'

let process file = 
  let ast = File.open_in file |> parse in
  begin if O.get ast_out then
    let str = ContentAst.show_p ast in
    String.nreplace ~str ~sub:"Ast.Wrap." ~by:""
    |> print_endline
  end
  (* begin if O.get typecheck then
    ast |> Check.type_expr
  end *)

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | lst :: _ -> 
    process lst
