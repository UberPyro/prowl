(* open Batteries

open Prowl
open Cli

(* let ast_out = flag "dump ast" "ast" *)
let typecheck = flags "typecheck program" "check" 't'

let process file = 
  let cst = Parse.file file in
  match cst.program with
  | None -> 
    cst.errors
    |> List.map Tree_sitter_run.Tree_sitter_error.to_string
    |> String.concat "\n\n"
    |> print_endline
  | Some e -> 
    let ast = Abstract.source_file e in
    let _ (* hir *) = Hir.Ast_to_hir.expr ast in
    if O.get typecheck then 
      () (* todo *)
    else () (* todo *)

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | lst :: _ -> 
    process lst *)
  