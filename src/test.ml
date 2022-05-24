open Batteries
open Alcotest

open Util

let tests = [
  "fundamentals", [
    "lit1", "4\n5\nhi"
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

let test () = 
  begin
    let+ group, lst = tests in
    group, let+ fn, out = lst in
    check_file group fn out
    |> test_case fn `Quick
  end
  |> run "Prowl Integration Tests"
