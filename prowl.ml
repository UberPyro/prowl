open Batteries
module O = BatOptParse.Opt

open Lib
open Cli

open Gen
open Ast

let lex_out = flag "output lexemes" "lex"
let ast_out = flag "output ast as ocaml ADTs" "ast"
let span_out = flag "output code spans with ast" "span"
let interpret = flags "interpret sources" "interpret" 'i'

let compile = List.iter begin fun file -> 
  if O.get lex_out then File.open_in file |> lex;
  let ast = parse (File.open_in file) in
  begin if O.get ast_out then
    (if O.get span_out then span_flag := true);
    let str = show_program ast in
    String.nreplace ~str ~sub:"Ast." ~by:""
    |> print_endline end;
  begin if O.get interpret then
    match Interpret.(program null_st) ast with
      | Some v -> 
        List.rev_map Interpret.string_of_v v.stk
        |> List.iter print_endline
      | None -> print_endline "rejected" end
end

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | lst -> compile lst
