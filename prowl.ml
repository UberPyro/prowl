open Batteries
module O = BatOptParse.Opt

open Lib
open Interpret
module L = Eval.LazySearch
module R = Run(L)
open Cli

open Gen
open Ast

let lex_out = flag "output lexemes" "lex"
let ast_out = flag "output ast as ocaml ADTs" "ast"
let span_out = flag "output code spans with ast" "span"
let interpret = flags "interpret sources" "interpret" 'i'
(* let no_std = flag "don't search for the std" "wstd" *)

let compile file args = 
  if O.get lex_out then File.open_in file |> lex;
  let ast = parse (File.open_in file) in
  begin if O.get ast_out then
    let () = if O.get span_out then span_flag := true in
    let str = show_program ast in
    String.nreplace ~str ~sub:"Ast." ~by:""
    |> print_endline end;
  begin if O.get interpret then try
      Interpret.S.(restack args init)
      |> R.program (Build.endow "std" ast)
      |> L.unsafe_cut
      |> Interpret.S.s
      |> List.rev_map V.show
      |> List.iter print_endline with
    | L.Rejected -> print_endline "rejected" end

let () = match P.parse_argv op with
  | [] -> P.usage op ()
  | lst :: args -> compile lst (List.map (fun x -> V.VStr x) args)
