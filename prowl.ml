open Batteries
module O = BatOptParse.Opt

open Lib
open Interpret
open Cli

open Gen
open Ast

let lex_out = flag "output lexemes" "lex"
let ast_out = flag "output ast as ocaml ADTs" "ast"
let span_out = flag "output code spans with ast" "span"
let interpret = flags "interpret sources" "interpret" 'i'
(* let no_std = flag "don't search for the std" "wstd" *)
let test = flag "run tests" "test"

let compile file args = 
  if O.get lex_out then File.open_in file |> lex;
  let ast = parse (File.open_in file) in
  begin if O.get ast_out then
    let () = if O.get span_out then span_flag := true in
    let str = show_program ast in
    String.nreplace ~str ~sub:"Ast." ~by:""
    |> print_endline end;
  begin if O.get interpret then match
      ast
      (* |> Build.endow "std" *)
      |> Interpret.(program {init_st with stk=args})
      |> LazyList.get with
      | Some (v, _) -> 
        List.rev_map Interpret.string_of_v v.stk
        |> List.iter print_endline
      | None -> print_endline "rejected" end

let () = match P.parse_argv op with
  | _ when O.get test -> Test.test ()
  | [] -> P.usage op ()
  | lst :: args -> compile lst (List.map (fun x -> VStr x) args)
