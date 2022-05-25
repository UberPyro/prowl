open Batteries
open Alcotest

open Lib
open Util

let tests = [
  "fundamentals", [
    "lit1", "4\n5\nhi";
    "arith1", "7";
    "arith2", "5";
  ]; 
  "bindings", [
    "as1", "11";
    "let1", "6";
    "let-func", "25";
    "let-as", "2";
    "let-sect", "4";
    "compose", "3";
    "sect1", "1";
    "sect-full", "20"; 
    "sect-left", "2"; 
    "sect-right", "1";
  ]; 
  "patterns", [
    "cat", "9";
    "left", "13";
    "right", "0";
    "pair", "4";
    "capture-direct", "7";
    "capture-indirect", "7";
    "capture-fun", "2";
    "nest-capture-pair", "2";
    "nest-either-pair", "1";
    (* "long-pair", "5"; *)  (* FAILING :( *)
    "const-int", "1";
    "const-int-reject", "rejected";
    "const-str", "0";
  ]; 
  "flow", [
    "cat", "5";
    "alt", "0";
    "alt-handle", "1";
    "alt-rejected", "rejected";
    "alt-greedy", "0";
    "case", "2";
    "intersect", "2";
    "n-times", "7\n23";
    "opt", "3";
    "opt-handle", "2";
    "star", "6";
    "star-greedy", "1";
    "plus", "3";
    "plus-reject", "rejected";
    "alt-cut-accepted", "0";
    "alt-cut", "rejected";
    "alt-cut-handle", "1";
    "case-rel", "8";
    "case-rel2", "11";
    "case-cut", "rejected";
    "star-cut", "rejected";
    "star-rel", "5";
    "inversion", "6";
    "inversion-rejected", "rejected";
  ]; 
  "combinators", [
    "simple", "44";
    "compound", "3";
  ];
  "modules", [
    "access", "3";
    "open", "7";
  ]
]

let run_file fname = 
  File.open_in ("test/" ^ fname ^ ".prw")
  |> Gen.parse
  |> Interpret.(program init_st)
  |> LazyList.get
  |> function
  | Some (v, _) -> 
    List.rev_map Interpret.string_of_v v.stk
    |> String.concat "\n"
  | None -> "rejected"

let check_file group file output () = 
  run_file (Printf.sprintf "%s/%s" group file)
  |> check string "outputs match" output

let () = 
  begin
    let+ group, lst = tests in
    group, let+ fn, out = lst in
    check_file group fn out
    |> test_case fn `Quick
  end
  |> run "Prowl Integration Tests"
